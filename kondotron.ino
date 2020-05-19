#define CCD_PIN 10
#define LED_PIN 13
#define PIN_1 2
#define PIN_6 3
#define PIN_10 4
#define PIN_11 5
void setup() {
  pinMode(CCD_PIN,OUTPUT);
  pinMode(LED_PIN,OUTPUT);
  pinMode(PIN_1,OUTPUT);
  pinMode(PIN_6,OUTPUT);
  pinMode(PIN_10,OUTPUT);
  pinMode(PIN_11,OUTPUT);
  Serial.begin(9600);
  Serial.print("PRESS_'s'_TO_START_MEASURE\n");
}

void loop() {
  int t;
  int sw;
  sw = Serial.read();

  if(sw == 's'){
    Serial.print("START_MEASURE\n");
    Serial.print("PRESS_'q'_TO_STOP\n");

    while(1){
      t++;
      setHome();
      delay(60000);//ホームポジに戻るまで1分待つ
      for(int i=0;i<10;i++){
        takePhoto();
        delay(11000);//露光時間10秒+1秒待つ
        rotateTable();
        delay(5000);//余裕を持って5秒待つ
      }
      delay(440000);//10分になる(600-160)秒後まで待つ
      if(t == 1000){
      break;
    }
    }
    
  }
  if(sw == 'q'){
    digitalWrite(LED_PIN,LOW);
    digitalWrite(CCD_PIN,LOW);
    digitalWrite(PIN_1,LOW);
    digitalWrite(PIN_6,LOW);
    digitalWrite(PIN_10,LOW);
    digitalWrite(PIN_11,LOW);
    Serial.print("STOP_MEASURE\n");
    Serial.print("PRESS_'s'_TO_START_MEASURE\n");
  }
  if(sw == '1'){
    digitalWrite(PIN_1,HIGH);
    Serial.println("①_HIGH");
  }
  if(sw == '2'){
    digitalWrite(PIN_6,HIGH);
    Serial.println("⑥_HIGH");
  }
  if(sw == '3'){
    digitalWrite(PIN_10,HIGH);
    Serial.println("⑩_HIGH");
  }
  if(sw == '4'){
    digitalWrite(PIN_11,HIGH);
    Serial.println("⑪_HIGH");
  }

  if(sw == '6'){
    digitalWrite(PIN_1,LOW);
    Serial.println("①_LOW");
  }
  if(sw == '7'){
    digitalWrite(PIN_6,LOW);
    Serial.println("⑥_LOW");
  }
  if(sw == '8'){
    digitalWrite(PIN_10,LOW);
    Serial.println("⑩_LOW");
  }
  if(sw == '9'){
    digitalWrite(PIN_11,LOW);
    Serial.println("⑪_LOW");
  }

}
void takePhoto(){
      digitalWrite(CCD_PIN, HIGH);//撮影開始
      digitalWrite(LED_PIN,HIGH);
      delay(1000);
      digitalWrite(CCD_PIN, LOW);
      digitalWrite(LED_PIN, LOW); 
}

void setHome(){
      digitalWrite(PIN_1,HIGH);
      digitalWrite(PIN_10,LOW);
      digitalWrite(PIN_11,LOW);//回転設定
      delay(1000);
      digitalWrite(PIN_6,LOW);//回転実行
      delay(1000);
      digitalWrite(PIN_6,HIGH);
}

void rotateTable(){
      digitalWrite(PIN_1,LOW);
      digitalWrite(PIN_10,LOW);
      digitalWrite(PIN_11,LOW);//回転設定
      delay(1000);
      digitalWrite(PIN_6,LOW);//回転実行
      delay(1000);
      digitalWrite(PIN_6,HIGH); 
}
