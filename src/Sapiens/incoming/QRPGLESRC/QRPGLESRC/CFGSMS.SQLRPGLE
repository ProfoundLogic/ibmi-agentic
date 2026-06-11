     dflgStatus        s              1    inz('Y')

      *entry
     dCFGSMS           pr                  extpgm('CFGSMS')
     d inStatus                      10
     dCFGSMS           pi
     d inStatus                      10

      /Free
       Select;
       When inStatus = '*DISABLED';
          flgStatus = ' ';
       Endsl;
       Exec sql SET OPTION commit=*none;
       Exec sql UPDATE ssypf set syfl12 = (substr(syfl12,1,6) concat
        :flgStatus concat substr(syfl12,8,3));
       *inlr = *on;
      /End-free
