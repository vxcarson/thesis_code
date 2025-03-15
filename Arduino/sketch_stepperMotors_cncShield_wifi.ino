/*
This is a sketch using a CNC shield to controlling two stepper motors in the X and Y position.
This sketch is designed to use a wifi connection for communication

Created April 18, 2024 by Carson Evans
*/


#include <AccelStepper.h>
#include <WiFiS3.h>
#include <SPI.h>
#include <WiFiUdp.h>
#include "arduino_secrets.h"
#include "Arduino_LED_Matrix.h"   // Include the LED_Matrix library
#include "frames.h"               // Include a header file containing some custom icons


// Define the stepper motor connections
#define X_STEP_PIN 2
#define X_DIR_PIN 5
#define Y_STEP_PIN 3
#define Y_DIR_PIN 6
#define ENABLE_PIN 8

///////please enter your sensitive data in the Secret tab/arduino_secrets.h
char* ssid = SECRET_SSID;    // your network SSID (name)
char* pass = SECRET_PASS;    // your network password (use for WPA, or as a key for WEP)
int status = WL_IDLE_STATUS;  // the WiFi radio's status
unsigned int localPort = 8888;

// Number of seconds to wait for the wifi to reconnect
const int WIFI_DISCONNECT_COUNTER_LIMIT = 10;

// Storage for combining and displaying frames with the LED matrix
// (using 3 unsigned longs to store 96 bits)
unsigned long current_LED_display[3] = {0, 0, 0};

// Add a device ID or name to differentiate multiple devices
//const char* deviceID = "CQM2";  // Change this for each Arduino

// WiFiUDP Udp;
// bool broadcastRecieved = false;

ArduinoLEDMatrix matrix;

unsigned long previousMillis = 0;        // Stores the last time the event was updated
const long interval = 100;              // Interval at which to run (milliseconds)

WiFiServer server(80);

// Create instances of the stepper motor class
AccelStepper stepperX(AccelStepper::FULL4WIRE, X_STEP_PIN, X_DIR_PIN);
AccelStepper stepperY(AccelStepper::FULL4WIRE, Y_STEP_PIN, Y_DIR_PIN);

void setup() {
  // Set up the serial communication
  Serial.begin(9600);

  // Set the maximum speed and acceleration for each stepper motor
  stepperX.setMaxSpeed(500);
  stepperX.setAcceleration(500);

  stepperY.setMaxSpeed(500);
  stepperY.setAcceleration(500);

  matrix.begin();
  matrix.loadFrame(danger);

  // This is for disabling the motors when not in use
  pinMode(ENABLE_PIN, OUTPUT);
  digitalWrite(ENABLE_PIN, HIGH); // start with them disabled

  // Comment out if not connected to computer
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }
  delay(2000);

  Serial.println("\n\nSetting up...");

  String fv = WiFi.firmwareVersion();
  if (fv < WIFI_FIRMWARE_LATEST_VERSION) {
    Serial.println("Please upgrade the firmware");
  }
  // attempt to connect to WiFi network:
  while (status != WL_CONNECTED) {
    Serial.print("Attempting to connect to WPA SSID: ");
    Serial.println(ssid);
    // Connect to WPA/WPA2 network:
    status = WiFi.begin(ssid);

    // wait 10 seconds for connection:
    delay(10000);
  }

  // Display IP address in binary on the LED matrix
  loadIPAddressToLEDMatrix();

  // you're connected now, so print out the data:
  //Serial.print("You're connected to the network");
  //printCurrentNet();

  // Report the connection
  Serial.print("Connected to ");
  Serial.println(ssid);
  Serial.print("IP address: ");
  Serial.println(WiFi.localIP());

  server.begin();  // Start the HTTP server on port 80
  //Udp.begin(8888);  // Start UDP for broadcasting address on port 8888
  Serial.println("Server started");

  //broadcastIP();

}

void loop() {
  
  // Reset microcontroller if it can't reconnect to wifi
  int Wifi_disconnect_counter = 0;
  while (WiFi.status() != WL_CONNECTED) {
    
    Wifi_disconnect_counter++;
    
    if (Wifi_disconnect_counter >= WIFI_DISCONNECT_COUNTER_LIMIT) {
      setup();
    }
    
    //Wait a second for wifi to reconnect
    delay(1000);
  }


  WiFiClient client = server.available();   // Listen for incoming clients
  if (client) {
    String request = "";

    // broadcastRecieved = true; // we now know the broadcast was recieved

    while (client.connected()) {
      if (client.available()) {
        char c = client.read();
        Serial.write(c);
        if (c == '\n') {
          if (request.length() > 0 && request.startsWith("GET /")) {
            handleRequest(client, request);
            break;  // Exit after handling the request
          }
          request = "";  // Clear the request string for the next line
        }
        else if (c != '\r') {
          request += c;  // Add any non-return characters to the request string
        }
      }
    }
    client.stop();
    Serial.println("Client Disconnected.");
  }

// if (!broadcastRecieved){
//       broadcastIP();
//       delay(10000);
//     }

}

void handleRequest(WiFiClient client, String request) {
  Serial.print("Handling request: ");
  Serial.println(request);

  int firstSpace = request.indexOf(' ');
  int lastSpace = request.lastIndexOf(' ');
  String path = request.substring(firstSpace + 1, lastSpace);
  path.trim();  // Remove any whitespace or newlines

  Serial.print("Path: ");
  Serial.println(path);

  String message = "";
  
  digitalWrite(ENABLE_PIN, LOW); // Ensure motors are enabled

  if (path.startsWith("/X")) {
    int steps = path.substring(2).toInt();  // Extract the number of steps
    
    stepperX.move(steps);
    stepperX.runToPosition();

    message += "Moved X motor ";
    message += String(steps);
    message += " steps.";

    Serial.println(message);
    sendHttpResponse(client, 200, message);
  }
  else if (path.startsWith("/Y")) {
    int steps = path.substring(2).toInt();  // Extract the number of steps

    stepperY.move(steps);
    stepperY.runToPosition();

    message += "Moved Y motor ";
    message += String(steps);
    message += " steps.";

    Serial.println(message);
    sendHttpResponse(client, 200, message);
  }
  else {
    Serial.print("Error: Invalid command: ");
    Serial.println(path);
    sendHttpResponse(client, 400, path);
  }
  Serial.println("Disable motors");
  digitalWrite(ENABLE_PIN, HIGH); // Disable the stepper drivers
}


void sendHttpResponse(WiFiClient client, int statusCode, String message) {
  // Standard HTTP response starts with a status line including the status code and a descriptive text
  client.print("HTTP/1.1 ");
  client.print(statusCode);
  client.print(" ");

  switch (statusCode) {
    case 400:
      client.println("Bad Request");
      break;
    case 404:
      client.println("Not Found");
      break;
    case 500:
      client.println("Internal Server Error");
      break;
    case 501:
      client.println("Not Implemented");
      break;
    default:
      client.println("OK"); // Default to 200 OK if not otherwise specified
  }
}

// void broadcastIP() {
//   IPAddress ip = WiFi.localIP();
//   String ipString = String(deviceID) + ": " +
//                     String(ip[0]) + '.' + 
//                     String(ip[1]) + '.' + 
//                     String(ip[2]) + '.' + 
//                     String(ip[3]);
//   Udp.beginPacket(IPAddress(255, 255, 255, 255), localPort);  // broadcast address
//   Udp.write(ipString.c_str());
//   Udp.endPacket();
//   Serial.println("Broadcasting IP Address");
// }

void loadIPAddressToLEDMatrix() {

  IPAddress ip = WiFi.localIP();
  for (int i = 0; i < 4; i++) {
    unsigned char value = ip[i];
    for (int bit = 0; bit < 8; bit++) {
      // Calculate row index (bits 2^0 are on row 0 and bits 2^7 are on row 7)
      int row = bit;

      // Find which 32-bit storage value it falls in. val = int( (row*length + column)/32 )
        int nth_32bit = (row*12 + 8+i)/32;
        // position (from the left) in said 32-bit storage value
        int position_32bit = 31 - (row*12 + 8+i)%32;
      
      // Check if the two bits don't match and if so flip the bit in current_LED_display
      if (bool(value & (1<<bit)) != bool(current_LED_display[nth_32bit] & ((1UL<<position_32bit)))) {
        current_LED_display[nth_32bit] ^= (1UL << position_32bit);
      }
    }
  }

  matrix.loadFrame(current_LED_display);
}


