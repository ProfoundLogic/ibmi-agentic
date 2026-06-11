     H Copyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXBADCHR2 - FIXES BAD CHAR IN ACC DESC
     H*****************************************************************
   H*===============================================================*
   H*  Created   : 07/30/13                                         *
   H*  Programmer: C:180                                            *
   H*  Support   : CI114 82954                                      *
   H*===============================================================*
     H** REVISIONS:
     H*                                                               *
     FWMCMP     UF   E             DISK    INFDS(CMPDS)
     FWMCMP@BK  O    E             DISK    USROPN RENAME(PCM01:BK1)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D CMPDS           DS
     D  CMRRN                397    400B 0
      *
     D CLM#            DS
     D  CMCO#
     D  DASH1                         1    INZ('-')
     D  CMFND
     D  DASH2                         1    INZ('-')
     D  CMFYR
     D  DASH3                         1    INZ('-')
     D  CMCASE
     D
     D COUNT           S              7  0 INZ
     D COUNTX          S              7  0 INZ
     D UPDFLG          S              1
     D BKDTA           S              1
     D @BK             S               N   INZ(*OFF)
     D HX              S            264    INZ
     D I               S              5  0 INZ
     D ACCDESC         S             72    INZ

     C     *ENTRY        PLIST
     C                   PARM                    UPDFLG
     C                   PARM                    BKDTA



      /FREE
       *IN95 = UPDFLG = 'U';
       @BK = BKDTA;
       IF @BK;
          OPEN WMCMP@BK;
       ENDIF;
       EXCEPT HDG;
       READ WMCMP;

       DOW   NOT %EOF(WMCMP);
           HX = *BLANKS;
           HX = CMACDS + CMACD2 + CMACD3;
           EXSR SRHEX;
           CMACDS = %SUBST(HX:1:60);
           CMACD2 = %SUBST(HX:61:60);
           CMACD3 = %SUBST(HX:121:144);

           IF *IN78 and *IN95;
              Update PCM01;
              COUNT = COUNT + 1;
           ENDIF;
       READ WMCMP;
       ENDDO;

           IF COUNTX > *ZERO;
              IF *INOF;
                  EXCEPT HDG;
              ENDIF;
              EXCEPT TOT1;
           ENDIF;

           IF COUNT > *ZERO;
              IF *INOF;
                  EXCEPT HDG;
              ENDIF;
              EXCEPT TOT2;
           ENDIF;
       IF @BK;
          CLOSE WMCMP@BK;
       ENDIF;
       *INLR = *ON;
       //===================================================================
       // Remove Bad Characters;
       //===================================================================
       BEGSR SRHEX;

             *IN77 = *ON;
             *IN78 = *OFF;

             FOR I = 1 TO 144;

                 IF %SUBST(HX:I:1) < X'40' or
                    %SUBST(HX:I:1) = X'41' or
                    %SUBST(HX:I:1) > X'F9';
                       IF *IN77;
                            IF *INOF;
                               EXCEPT HDG;
                            ENDIF;
                            EXSR   GETACCDESC;
                            EXCEPT DET;
                            *IN77 = *OFF;
                            *IN78 = *ON;
                            COUNTX = COUNTX + 1;
                            IF @BK;
                                WRITE BK1;
                            ENDIF;
                       ENDIF;

                       %SUBST(HX:I:1)=  X'40';

                 ENDIF;

             ENDFOR;
       ENDSR;
       //===================================================================
       // Get accident description
       //===================================================================
       BEGSR GETACCDESC;
             SELECT;
               WHEN I <= 60;
                    ACCDESC = CMACDS;
               WHEN I <= 120;
                    ACCDESC = CMACD2;
               WHEN I <= 192;
                    ACCDESC = %SUBST(CMACD3:1:72);
               WHEN I <= 264;
                    ACCDESC = %SUBST(CMACD3:73:72);
               ENDSL;
       ENDSR;
      /END-FREE
     OQSYSPRT   E            HDG              01
     O                                              'RECORDS WITH BAD HEX CHAR'
     O          E            HDG         2
     O                                              'CLAIM NUMBER '
     O                                           29 'RRN'
     O          E            DET         1
     O                       CLM#
     O                       CMRRN         Z  +0001
     O                       ACCDESC           +001
     O          E            TOT1        2  1
     O                                              'TOTAL RECORDS WITH BAD'
     O                                         +001 'CHARACTERS...:'
     O                       COUNTX        4     +1
     O          E            TOT2        2  1
     O                                              'TOTAL RECORDS FIXED......:'
     O                       COUNT         4     +1
