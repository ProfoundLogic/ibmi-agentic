     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to update rate field and hazard group based on class.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WPIPOLPOP (Payroll edit window) - Called from WTPOLQTE and WTPOLDTL.
     h*  Calls to:
     h*
     h*  Created on 10/12/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h* 04/10/13 C:65  Project: SY266 Redmine:2700
     h*                Apply LCM to payroll rate.
     h*
     h*
     h**********************************************************************
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)

      /copy sPrDate
      /copy sPrAtrium

      * *ENTRY
     DQuotePayrollRt   pr                  ExtPgm('WTPOLQTE12')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DQuotePayrollRt   pi
     D inRRN                          9  0
     D inData                       512
     D outJSON                    32767
     D rtnCode                        1
     D*
     d numEdtCde       c                   const('N')
     D*
     D xxRTCD          s              3    inz
     D xxClass         s              5  0 inz
     D xxDate          s              7  0 inz
     D xxType          s              1    inz
     D xxEfdt          s              7  0 inz
     D clsStr          s              5    inz
     d dateStr         s             10    inz
     d dateStr2        s             10    inz
     d slash1          s              2  0 inz
     d slash2          s              2  0 inz
     D numberDate      s              8  0 inz

     d* Effective Date
     d SrEffd          pr             7  0
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)

     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    xxRTCD
     C                   Kfld                    xxClass
     C                   Kfld                    xxEfDt
     C     key1WMPRL1    Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    xxRTCD
     C                   Kfld                    xxClass
     C*

      /Free

       Chain(n) inRRN PEL0P;

       xxRTCD  = ' ';
       xxClass = 0;
       xxDate  = 0;
       xxEfdt  = 0;
       outJSON = ' ';

       xxRTCD = %subst(inData:%scan(',RTCD:':inData:1)+6:
        %scan(',CLS:':indata:1)-%scan(',RTCD:':inData:1)-6);

       clsStr = %subst(inData:%scan(',CLS:':inData:1)+5:
        %scan(',DATE:':indata:1)-%scan(',CLS:':inData:1)-5);
       xxClass = %int(%trim(clsStr));

       dateStr = %subst(inData:%scan(',DATE:':inData:1)+6:10);

       If %scan('/':dateStr:1) <> 0;
          slash1 = %scan('/':dateStr:1);
          slash2 = %scan('/':dateStr:slash1+1);
          dateStr2 = %subst(dateStr:1:slash1-1) +
           %subst(dateStr:slash1+1:slash2-slash1-1) + %subst(dateStr:slash2+1);
       Else;
          dateStr2 = dateStr;
       Endif;

       numberDate = %int(%trim(dateStr2));
       xxDate = dtCYMD(dtCheck(numberDate));

       xxType = 'P';

       xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:xxDate:xxType:
        xxClass);
       Setgt keyWMPRL1 PPR01;
       Readpe key1WMPRL1 PPR01;
       If not %eof;

          Exsr srLCM;
          BuildJSON(outJSON :'payrollRate' : %editc(PRRT:numEdtCde));
          BuildJSON(outJSON :'hazard' : PRHZGR );
          If PRArfl = 'Y' and ELStat = 'Y';
             BuildJSON(outJSON :'aRate' : 'Y' );
          Else;
             BuildJSON(outJSON :'aRate' : 'N' );
          Endif;

       Endif;

       rtnCode = ' ';
       Return;
      /End-Free
     C**********************************************************************************
     C*
     C* Call SRLCM to update the payroll rate.
     C*
     C**********************************************************************************
     C     srLCM         Begsr
     C*
     C                   Z-ADD     1             XXLCM
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   xxClass       QQCLAS
     C                   MOVE(P)   xxEFDT        QQEFDT
     C                   CALL      'SRLCM'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQEFDT            7
     C                   PARM      *BLANKS       QQLCM             5
      *
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
      *
     C     PRRT          MULT(H)   XXLCM         PRRT
     C*
     C                   Endsr                                                  srLCM

       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0
     d   inFND                        3  0
     d   inFYR                        3  0
     d   inEMP#                       9  0
     d   inDIV                        5  0
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
     d*
     d* SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d*
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
     d*

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
       //***************************************************************************//
