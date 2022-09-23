
String inputString = "";
String str = "";

String Xdir = "";
String Xstep = "";

String Ydir = "";
String Ystep = "";

String laserON = "";
    
int stpX = 0;
int stpY = 0;
int laserPower = 0;

bool stringComplete = false;
bool enabled = false;

const int PulseX = 6;
const int dirX = 13;
const int enableX = 12;

const int PulseY = 5;
const int dirY = 8;
const int enableY = 7;

const int laser = 3;

void setup() {

  pinMode(dirX, OUTPUT);
  pinMode(enableX, OUTPUT);

  digitalWrite(dirX, LOW);
  digitalWrite(enableX, HIGH);
  
  pinMode(dirY, OUTPUT);
  pinMode(enableY, OUTPUT);

  digitalWrite(dirY, LOW);
  digitalWrite(enableY, HIGH);

  analogWrite(PulseX, 0);
  analogWrite(PulseY, 0);
  analogWrite(laser, 90);

  
  Serial.begin(9600);
  inputString.reserve(200);

}

void loop() {
  if (stringComplete) {
    int delim = inputString.indexOf("m2");
    
    laserON = inputString.substring(0,3);
    Xdir = inputString.substring(3,4);
    Xstep = inputString.substring(4,delim);
    Ydir = inputString.substring(delim + 2,delim + 3);
    Ystep = inputString.substring(delim + 3);

    stpX = Xstep.toInt();
    stpY = Ystep.toInt();
    laserPower = laserON.toInt();


    Serial.print("X:");
    Serial.print(Xdir);
    Serial.print(":");
    Serial.print(Xstep);

    Serial.print("  Y:");
    Serial.print(Ydir);
    Serial.print(":");
    Serial.print(Ystep);

    if (stpX == 0 && stpY == 0) {
        analogWrite(laser, laserPower);
    }

    if (stpX != 0) {
        analogWrite(PulseX, 0);
        digitalWrite(enableX, LOW);
        delay(2);
      if (Xdir.equals("r")) {
        digitalWrite(dirX, LOW);
      }
      else if (Xdir.equals("l")){
        digitalWrite(dirX, HIGH);
      }
      delay(2);
      analogWrite(laser, laserPower);
      analogWrite(PulseX, 127);
      delay(stpX);
      analogWrite(laser, 0);
      analogWrite(PulseX, 0);
      delay(2);
      digitalWrite(enableX, HIGH);
      delay(2);
      Serial.print("X:ok\n");
    }

    delay(100);
  
    if (stpY != 0) {        
      analogWrite(PulseY, 0);
      delay(2);
      digitalWrite(enableY, LOW);
      delay(2);
      if (Ydir.equals("r")) {
        digitalWrite(dirY, LOW);
      }
      else if (Ydir.equals("l")){
        digitalWrite(dirY, HIGH);
      }
      delay(2);
      analogWrite(laser, laserPower);
      analogWrite(PulseY, 127);
      delay(stpY);
      analogWrite(laser, 0);
      analogWrite(PulseY, 0);
      delay(2);
      digitalWrite(enableY, HIGH);
      delay(2);
      Serial.print("Y:ok\n");
    }
    inputString = "";
    stringComplete = false;
  }
}


void serialEvent() 
{
  while (Serial.available()) {
    char inChar = (char)Serial.read();
    inputString += inChar;
    if (inChar == '\n') {
      stringComplete = true;
    }
   }
}


