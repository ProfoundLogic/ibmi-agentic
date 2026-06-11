     hCopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h option(*nodebugio: *srcstmt)
     h DFTACTGRP(*no) BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: This program will write Cross Reference Circular
     h*               records from file WZXEP in WCIRP.
     h*
     h*  Called From: None
     h*  Calls to: None
     h*
     h*  Created on 07/18/2013 by C:282 for Redmine 3017
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwzexp     if   e             disk
     f                                     rename(pex01:pex0p)
     fwzexl1    if   e           k disk    usropn
     f                                     prefix(C:1)
     fwdell2    if   e           k disk    rename(pel01:pel00)
     f
     fwdell2q   if   e           k disk    rename(pel01:pel01)
     f                                     prefix(D:1)
     fwcirp     uf a e             disk
     f                                     rename(pir01:pir0p)
     fwzexl     uf   e           k disk    rename(pex01:pex02)
     f                                     prefix(X:1)
     fwcirl     uf   e           k disk
     f
      *
     dintCo#           s              3  0
     dintFnd           s              3  0
     dintEmp#          s              9  0
     dintOFnd          s              3  0
     dintOEmp          s              9  0
     dsvCo#            s              3  0
     dsvFnd            s              3  0
     dsvEmp#           s              9  0
     dsvOFnd           s              3  0
     dsvOEmp           s              9  0
     dcounter          s              4  0
      *
     dsv0FYR           s                   like(elfyr)
     dsv0PFDT          s                   like(elpfdt)
     dsv0DT            s                   like(eldt)
     dsv0Time          s                   like(eltime)
     dsv1FYR           s                   like(elfyr)
     dsv1PFDT          s                   like(elpfdt)
     dsv1DT            s                   like(eldt)
     dsv1Time          s                   like(eltime)

      /copy sPrRunCmd

      *
      /Free
       RunCmd('ARCOBJ OBJ(WZEXP) OBJTYPE(*FILE)':32);

       Setll *Start wzexp;
       Read wzexp;

       DoW Not %EOF(wzexp);

          counter = 1;
          intCo#  = EXco#;
          intFnd  = EXfnd;
          intEmp# = EXemp#;
          intOFnd = EXofnd;
          intOEmp = EXoemp;

       // To check Cross Reference Direct/Non-Direct Circular record exist
       // in file WZEXP.

       // Read the record in Low - High manner
          open wzexl1;

          Setll (intCo#:intFnd:intEmp#) pex01;
          Reade (intCo#:intFnd:intEmp#) pex01;
          Dow Not %EOF(wzexl1);
             If (intOFnd = CXfnd) And
                (intOEmp = CXemp#);

                IRco#  =  CXco#;
                IRfnd  =  CXfnd;
                IRemp# =  CXemp#;
                IRdiv  =  CXdiv;
                IRofnd =  CXofnd;
                IRoemp =  CXoemp;
                IRodiv =  CXodiv;
                Write  pir0p;
                Leave;
             Else;
                svCo#  = CXco#;
                svFnd  = CXfnd;
                svEmp# = CXemp#;

                If Counter = 1;
                  svOFnd = CXofnd;
                  svOEmp = CXoemp;
                EndIf;

       // Read the record in High - Low manner
                close wzexl1;
                open  wzexl1;

                Setgt  (intCo#:intFnd:intEmp#) pex01;
                Readpe (intCo#:intFnd:intEmp#) pex01;
                If (intOFnd = CXfnd) And
                   (intOEmp = CXemp#);

                   IRco#  =  CXco#;
                   IRfnd  =  CXfnd;
                   IRemp# =  CXemp#;
                   IRdiv  =  CXdiv;
                   IRofnd =  CXofnd;
                   IRoemp =  CXoemp;
                   IRodiv =  CXodiv;
                   Write  pir0p;
                   Leave;
                EndIf;

       // Read the record for Non Direct Circular cases.
                close wzexl1;
                open  wzexl1;

                Setll (svCo#:svFnd:svEmp#) pex01;
                Reade (svCo#:svFnd:svEmp#) pex01;
                If (svOFnd = CXfnd) And
                   (svOEmp = CXemp#);
                    Leave;
                EndIf;
                counter += 1;
             EndIf;
          EndDo;

       close wzexl1;

       Read wzexp;

       EndDo;

       RunCmd('ARCOBJ OBJ(WCIRP) OBJTYPE(*FILE)':32);

       // Start reading the Direct Circular and Non- Direct Circular record.
       Setll *Start wcirp;
       Read wcirp;

       DoW Not %EOF(wcirp);

       // Retrieve the infomation for current employer.
       Setgt  (IRco#:IRfnd:IRemp#:IRdiv) pel00;
       Readpe (IRco#:IRfnd:IRemp#:IRdiv) pel00;
       If Not %EOF(wdell2);
          sv0FYR  = ELfyr;
          sv0PFDT = ELpfdt;
          sv0Dt   = ELdt;
          sv0Time = ELtime;
       EndIf;

       // Retrieve the infomation for old employer.
       Setgt  (IRco#:IRofnd:IRoemp:IRodiv) pel01;
       Readpe (IRCo#:IRofnd:IRoemp:IRodiv) pel01;
       If Not %EOF(wdell2q);
          sv1FYR  = DLfyr;
          sv1PFDT = DLpfdt;
          sv1Dt   = DLdt;
          sv1Time = DLtime;
       EndIf;

       // Verify and if condition satisfy delete the record from wzexp.
       Select;
       When sv0FYR > sv1FYR;
           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pex02;
           If %Found(wzexl);
             Delete pex02;
           EndIf;

           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pir01;
           If %Found(wcirl);
             Delete pir01;
           EndIf;
           Delete pir0p;

       When sv0FYR = sv1FYR And
            sv0PFDT > sv1PFDT;
           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pex02;
           If %Found(wzexl);
              Delete pex02;
           EndIf;

           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pir01;
           If %Found(wcirl);
             Delete pir01;
           EndIf;
           Delete pir0p;

       When sv0FYR = sv1FYR And
            sv0PFDT = sv1PFDT And sv0DT > sv1DT;
           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pex02;
           If %Found(wzexl);
              Delete pex02;
           EndIf;

           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pir01;
           If %Found(wcirl);
             Delete pir01;
           EndIf;
           Delete pir0p;

       When sv0FYR = sv1FYR And
            sv0PFDT = sv1PFDT And sv0DT = sv1DT And
            sv0Time > sv1Time;
           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pex02;
           If %Found(wzexl);
              Delete pex02;
           EndIf;

           Chain  (IRco#:IRofnd:IRoemp:IRodiv) pir01;
           If %Found(wcirl);
             Delete pir01;
           EndIf;
           Delete pir0p;

       Other;
       EndSl;

       Read wcirp;

       EndDo;

       *InLr = *On;

      /End-Free
