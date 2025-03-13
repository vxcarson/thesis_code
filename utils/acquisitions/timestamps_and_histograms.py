import time
import logging
import sys
import numpy as np
import concurrent.futures
from typing import Any, Dict, Iterable, List
from utils.common import zmq_exec, dlt_exec
from utils.acquisitions.timestamps import wait_end_of_timestamps_acquisition, close_timestamps_acquisition

logger = logging.getLogger(__name__)

def open_timestamps_acquisition_with_names(
    tc, dlt, tc_address, channels, fmt, output_dir, file_name, with_ref_index
):
    acquisitions_id = {}

    for channel in channels:
        # Reset error counter
        zmq_exec(tc, f"RAW{channel}:ERRORS:CLEAR")

        # Tell the DataLink to start listening and store the timestamps in a file.
        # Command: start-save --address <time controller ip> --channel <1-4> --filename <relative/full path> --format <bin|ascii>
        # Returns the ID of the started acquisition

        ext = "bin" if fmt == "bin" else "txt"

        filepath = output_dir / f"{file_name}_C{channel}.{ext}"
        filepath_escaped = str(filepath).replace("\\", "\\\\")

        command = f'start-save --address {tc_address} --channel {channel} --filename "{filepath_escaped}" --format {fmt}'
        if with_ref_index:
            command += " --with-ref-index"
        answer = dlt_exec(dlt, command)

        acquisitions_id[channel] = answer["id"]

        # Start transfer of timestamps
        zmq_exec(tc, f"RAW{channel}:SEND ON")

    return acquisitions_id

def wait_end_of_hist_and_tstamp_acquisition(tc, dlt, acquisitions_id, timeout=30):
    """Wait until histogram and timestamp acquisitions are all done or encounered an error."""

    SLEEP_TIME = 1  # time between each end of acquisition check
    NATURAL_INACTIVITY = 1  # allowed natural inactivity after end of acquisition

    timeout = max(timeout, SLEEP_TIME + 1, NATURAL_INACTIVITY)

    done = {channel: False for channel in acquisitions_id}

    try:
        number_of_record = int(zmq_exec(tc, "REC:NUMber?"))
    except ValueError:
        number_of_record = None
        timeout += NATURAL_INACTIVITY

    while not all(done.values()):
        time.sleep(1)

        acquisition_playing = zmq_exec(tc, "REC:STAGe?").upper() == "PLAYING"

        active_channels_status = {
            channel: dlt_exec(dlt, f"status --id {acqu_id}")
            for channel, acqu_id in acquisitions_id.items()
            if not done[channel]
        }

        max_acqu_count = max(
            status["acquisitions_count"] for status in active_channels_status.values()
        )

        for channel, status in active_channels_status.items():
            # Consider channel acquisition done if an arror occured
            if status.get("error"):
                done[channel] = True
                continue

            if not acquisition_playing:

                # Infinite number of records...
                if not number_of_record:
                    # ... Channel properly registered all end of sub-acquisitions
                    #
                    # We wait that NATURAL_INACTIVITY is exceeded before we accept to beleive that
                    # max_acqu_count is indeed the final number recorded of sub-acquisitions across
                    # all channels.
                    if (
                        status["acquisitions_count"] > 0
                        and status["acquisitions_count"] == max_acqu_count
                        and status["inactivity"] > NATURAL_INACTIVITY
                    ):
                        done[channel] = True
                        continue

                # Channel properly registered the end of the acquisition
                elif status["acquisitions_count"] >= number_of_record:
                    done[channel] = True
                    continue

                # Channel did not receive any message for too long after end of acquisition
                if status["inactivity"] > timeout:
                    logger.error(f"[channel {channel}] timestamp transfert timeout")
                    done[channel] = True
                    continue


def acquire_timestamps_and_histograms(
    tc, dlt, tc_address, duration: int, channels: Iterable[int], fmt, output_dir_ts, file_name_ts, with_ref_index, bwid: int, bcount: int
):
    ### Configure the acquisition timer

    # Trigger RECord signal manually (PLAY command)
    zmq_exec(tc, "REC:TRIG:ARM:MODE MANUal")
    # Enable the RECord generator
    zmq_exec(tc, "REC:ENABle ON")
    # STOP any already ongoing acquisition
    zmq_exec(tc, "REC:STOP")
    # Record a single acquisition
    zmq_exec(tc, "REC:NUM 1")
    # Record for the request duration (in ps)
    zmq_exec(tc, f"REC:DURation {duration * 1e12}")

    acquisitions_id = open_timestamps_acquisition_with_names(
        tc, dlt, tc_address, channels, fmt, output_dir_ts, file_name_ts, with_ref_index
    )

    for i in channels:
        zmq_exec(tc, f"HIST{i}:BCOUnt {bcount}")  # Set histogram maximum bin count
        zmq_exec(tc, f"HIST{i}:BWID {bwid}")  # Set histogram bin width
        zmq_exec(tc, f"HIST{i}:FLUSh")  # Flush histogram

    zmq_exec(tc, "REC:PLAY")  # Start the acquisition
    print(f"Histogram and Timestamp Aquisition Started ({duration}s)")

    with concurrent.futures.ThreadPoolExecutor() as executor:
        future1 = executor.submit(visual_timer, duration)
        future2 = executor.submit(wait_end_of_hist_and_tstamp_acquisition, tc, dlt, acquisitions_id, timeout=30)
        
        # Wait for both functions to complete
        result1 = future1.result()
        result2 = future2.result()

    print(f"Aquisition Complete")
    
    # Get histogram data
    histograms = {i: eval(zmq_exec(tc, f"HIST{i}:DATA?")) for i in channels}

    success = close_timestamps_acquisition(tc, dlt, acquisitions_id)
  
    return [success, histograms]


def visual_timer(duration, flash_speed=2, update_interval=0.1, max_size=30):
    '''flash_speed=2 number of flashes per second
    update_interval=0.1 seconds between updating'''
    
    total_dashes = int( min(max_size, duration) )

    # Decrease update interval if flash_speed is faster
    update_interval = min(update_interval, 1/flash_speed)
    
    # Print initial dashes and return to start without new line
    print('-' * total_dashes, end='', flush=True)
    sys.stdout.flush()
 

    # Record keeping variables
    start_time = time.time()
    elapsed_time = time.time() - start_time
    onum = 0; # number of o's
    isFlashed = False;
    
    while elapsed_time < duration:
        percentage_complete = elapsed_time / duration
        dashes_to_overwrite = int(percentage_complete * total_dashes)
        
        # For flashspeed=2 it returns false for # <= time < #.5 and true #.5 <= time < #+1
        # That means it should flash every 0.5 seconds regardless of faster update speeds
        shouldFlash = np.mod(int(elapsed_time*flash_speed),2)

        # Need to update if there are more dashes to overwrite or we need to change the flashing dash
        if dashes_to_overwrite - onum or shouldFlash != isFlashed:
            overwrite = '\r' + 'o' * dashes_to_overwrite
            onum = dashes_to_overwrite

            if shouldFlash:
                overwrite += ' '
                isFlashed = True
            else:
                overwrite += '-'
                isFlashed = False

            # Return to the start of the line and overwrite with overwrite string
            sys.stdout.write(overwrite)
            sys.stdout.flush()
        

        # Wait for a bit
        time.sleep(update_interval)
        
        # Update elapsed_time
        elapsed_time = time.time() - start_time

    # Overwrite the line with 'o's at the end to ensure complete overwrite
    sys.stdout.write('\r' + 'o' * total_dashes + '\n')
    sys.stdout.flush()
