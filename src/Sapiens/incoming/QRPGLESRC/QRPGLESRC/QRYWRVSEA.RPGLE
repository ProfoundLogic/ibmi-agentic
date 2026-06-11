     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')

       Dcl-F WDELL1 Keyed Usage(*Input) Rename(pel01:pel01);
       Dcl-F QRYWRVSEAF Usage(*Delete:*Output) Rename(qry01:qry01);

       Dcl-Pr QryWrVsEa;
        inCo# Char(3);
        inFnd Char(3);
        inEmp Char(9);
        inFyr Char(3);
        inDate Char(8);
       End-Pr;
       Dcl-Pi QryWrVsEa;
        inCo# Char(3);
        inFnd Char(3);
        inEmp Char(9);
        inFyr Char(3);
        inDate Char(8);
       End-Pi;

     d SRPRT1          pr                  extpgm('SRPRT1')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##fyr                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##date                         8    const
     d ##amount                      11
     d ##amount2                     11
     d ##term                         1

       Dcl-S chrRtnAmount Char(11);
       Dcl-S chrRtnAmount2 Char(11);
       Dcl-S chrTerm Char(1);

      // Clear workfile
       Dou %Eof(QRYWRVSEAF);
          Read qry01;
          If %Eof;
             Leave;
          Endif;
          Delete qry01;
       Enddo;

       Setll (%Int(inCo#):%Int(inFnd):%Int(inFyr):%Int(inEmp)) pel01;
       Dou %Eof(WDELL1);
          Reade (%Int(inCo#):%Int(inFnd):%Int(inFyr):%Int(inEmp)) pel01;
          If %Eof;
             Leave;
          Endif;
          QRco# = ELco#;
          QRfnd = ELfnd;
          QRemp# = ELemp#;
          QRdiv = ELdiv;
          QRfyr = ELfyr;
          QRfaf = ELfaf;
          Select;
          When ELstam = *Zero;
             If ELfaf = 'A';
                QRwrtn = ELaapr;
             Else;
                QRwrtn = ELanpr;
             Endif;
          When ELstam > *Zero;
             QRwrtn = ELstam;
          Endsl;
          chrTerm = '1';
          SrPrt1(inCo#:inFnd:inFyr:inEmp:%Editc(ELdiv:'X'):inDate:
           chrRtnAmount:chrRtnAmount2:chrTerm);
          QREarn = %Dec(chrRtnAmount:11:2);
          Eval(h) QRearn = QRearn/100;
          Write qry01;
       Enddo;
       *Inlr = *On;
