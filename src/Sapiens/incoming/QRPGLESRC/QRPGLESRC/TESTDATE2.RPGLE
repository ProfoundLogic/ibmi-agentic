     d timeDelay       s              5s 0 Inz(15)
     d timeStamp       s               z
     d chrDate         s             10a
     d decDateMDYY     s              8s 0
     d decDateYYMD     s              8s 0
     d decDateCYMD     s              8s 0
     d chrTime         s              8a
     d decTime         s              6s 0

              timeStamp = %TimeStamp() + %Minutes(timeDelay);
              chrDate     = %Char(%Date(timestamp):*usa);
              chrTime     = %Char(%Time(timestamp):*hms);
              decDateMDYY = %Dec(%Date(timestamp):*usa);
              decDateYYMD = %Dec(%Date(timestamp):*iso);
              decDateCYMD = %Dec(%Date(timestamp):*cymd);
              decTime     = %Dec(%Time(timestamp):*hms);

              *inLR = *On;
