     H*=====================================================================
     H* SAPSTUBS - Stub implementations of Sapiens copybook procedures.
     H* PHASE 3 SCAFFOLDING - REVERSE-ENGINEERED.
     H* Returns safe defaults so WTAGTCFG can bind & launch without the
     H* real STBNDDIR/WTGRPCFG binding directories.
     H*=====================================================================
     H nomain
     H option(*nodebugio:*srcstmt)

      /COPY SPRGENERR
      /COPY SPRATRIUM
      /COPY SPRDATE
      /COPY SPRCOMPANY
      /COPY SPRusrprms
      /COPY SPRsql
      /COPY SCOPYCOMPR
      /COPY wprpolerr

     P Check_Email     B                   Export
     D Check_Email     PI             1N
     D  inEmail                      60
     D  inMessage                   200
      /free
       return *off;
      /end-free
     P Check_Email     E

     P CreateGenURL    B                   Export
     D CreateGenURL    PI           720
     D inPrefix                      50    const
     D inParm01                      50    options(*nopass) const
     D inParm02                      50    options(*nopass) const
     D inParm03                      50    options(*nopass) const
     D inParm04                      50    options(*nopass) const
     D inParm05                      50    options(*nopass) const
     D inParm06                      50    options(*nopass) const
     D inParm07                      50    options(*nopass) const
     D inParm08                      50    options(*nopass) const
     D inParm09                      50    options(*nopass) const
     D inParm10                      50    options(*nopass) const
     D inParm11                      50    options(*nopass) const
      /free
       return ' ';
      /end-free
     P CreateGenURL    E

     P CreateURL       B                   Export
     D CreateURL       PI           720
     D inProgram                     10    const
     D inTabName                     50    const
     D inParm01                      50    options(*nopass) const
     D inParm02                      50    options(*nopass) const
     D inParm03                      50    options(*nopass) const
     D inParm04                      50    options(*nopass) const
     D inParm05                      50    options(*nopass) const
     D inParm06                      50    options(*nopass) const
     D inParm07                      50    options(*nopass) const
     D inParm08                      50    options(*nopass) const
     D inParm09                      50    options(*nopass) const
     D inParm10                      50    options(*nopass) const
      /free
       return ' ';
      /end-free
     P CreateURL       E

     P DateTo7         B                   Export
     D DateTo7         PI             7  0
     D inDate                          D   Const
     D wResult         S              7  0
      /free
       wResult = %dec(%char(inDate:*iso0):7:0);
       return wResult;
      /end-free
     P DateTo7         E

     P dtCYMD          B                   Export
     D dtCYMD          PI             7  0
     D inNumDate8                     8  0 Const
      /free
       if inNumDate8 = 0;
          return 0;
       endif;
       if inNumDate8 >= 20000000;
          return inNumDate8 - 19000000;
       endif;
       return inNumDate8 - 18000000;
      /end-free
     P dtCYMD          E

     P getCompNam      B                   Export
     D getCompNam      PI            40
     D inCompNum                      3  0 Const
      /free
       return 'STUB Company ' + %char(inCompNum);
      /end-free
     P getCompNam      E

     P GetUserInfo     B                   Export
     D GetUserInfo     PI             3  0
     D inUserID                      10    const
     D outUserName                   50    options(*nopass)
     D outUserOutQ                   10    options(*nopass)
     D outUserStaff                   3    options(*nopass)
     D outUserType                    1    options(*nopass)
     D outUserLvl                     3  0 options(*nopass)
      /free
       if %parms() >= 2;
          outUserName = inUserID;
       endif;
       if %parms() >= 3;
          outUserOutQ = ' ';
       endif;
       if %parms() >= 4;
          outUserStaff = ' ';
       endif;
       if %parms() >= 5;
          outUserType = ' ';
       endif;
       if %parms() >= 6;
          outUserLvl = 0;
       endif;
       return 1;
      /end-free
     P GetUserInfo     E

     P IsValidFetch    B                   Export
     D IsValidFetch    PI             1N
     D inSqlState                     5A   const
      /free
       return inSqlState = '00000';
      /end-free
     P IsValidFetch    E

     P LoadActionBar   B                   Export
     D LoadActionBar   PI
      /free
       return;
      /end-free
     P LoadActionBar   E

     P systemDate      B                   Export
     D systemDate      PI              D
      /free
       return %date();
      /end-free
     P systemDate      E

     P systemTime      B                   Export
     D systemTime      PI              T
      /free
       return %time();
      /end-free
     P systemTime      E

     P TimeTo6         B                   Export
     D TimeTo6         PI             6  0
     D inTime                          T   Const
      /free
       return %dec(%char(inTime:*hms0):6:0);
      /end-free
     P TimeTo6         E

     P ValidDate       B                   Export
     D ValidDate       PI             1
     D inDate                         8  0
     D inMessageID                  100
      /free
       if inDate = 0;
          return '0';
       endif;
       return '1';
      /end-free
     P ValidDate       E
