     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
      //================================================================
     H/TITLE FIXAGTCHK - UPDATE AGT COMMISSION & CHECK TRANSACTION FILES
      //================================================================
      // This program is a reduced version of W3060R used because COBUL2
      //  was cleared and the payment records were not written to WMACP
      //
      // When testing, the following files should be duplicated to a
      //  test library that is in the LIBL just below QTEMP.
      //  (SYDUPFILE can help ease the process)
      // - WAR2P/L*
      // - WMACP/L*
      // - WSCKP/L*
      // - SMCOP/L*
      // - Check files in W3030R1
      //================================================================
      // Revisions:
      //
      //================================================================
     FWAR2L     UF   E           K DISK
     FWAC2L     UF   E           K DISK
     FWMACL7    UF A E           K DISK
      *________________________________________________________________
      *
      *  Bank Account Master
     FWSCKL     UF   E           K DISK
     FWMACL21   UF   E           K DISK
     F                                     RENAME(PAC01:PAC21)
      *________________________________________________________________
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     Di1               s              5  0
     DxxBul2           s                   like(ACbul2)
     DxxChk#           s                   like(ACchck)
     DxxCo#            s                   like(ACco#)
     DxxFnd            s                   like(ACfnd)
     DxxFyr            s                   like(ACfyr)
     DdsWMPHP        e ds                  extname(WMPHP) dim(999) qualified inz
     D/copy WPRBANK

     DW3021R           pr                  extpgm('W3021R')
     D outCo#                         3    const
     D outFnd                         3    const
     D outDate                        8    const
     D outCode                        2    const

     DW3030R1          pr                  extpgm('W3030R1')
     D outDate                        8    const
     D outCo#                         3    const
     D outFnd                         3    const

     DW3070C           pr                  extpgm('W3070C')
     D outCo#                         3    const
     D outDate                        8    const
     D outFnd                         3    const
     D outQue                        10    const
     D outUseCurDt                    1    const

     DW3080R           pr                  extpgm('W3080R')
     D outDate                        8    const
     D outCo#                         3    const
     D outFnd                         3    const

     C     *LIKE         DEFINE    ACPODT        XXDT
     C     *LIKE         DEFINE    R2FND         SVFND
     C     *LIKE         DEFINE    R2ACCT        SVACCT
     C     *LIKE         DEFINE    R2CHCK        SVCHK#
     C     *LIKE         DEFINE    ACSEQ#        SVSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C*  REQUEST COMMISSION PAYMENT KEY LIST
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AGENT COMMISSION FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    R2BUL2
     C*
     C*  CHECK SCHEDULE FILE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
      *
      *
      *  Bank Account Master File
      *
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVACCT
      *
     C     ACKY2         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C                   KFLD                    R2CHCK
     C*
     C     ACKY3         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         SVFND                          Save Fund
     C                   MOVE      *BLANK        SVACCT                         Save Account
     C                   Z-ADD     *ZERO         SVCHK#                         Save Check#
     C*
     C*  CHAIN TO THE CHECK SCHEDULE FILE, WE MUST HAVE A RECORD
     C*  FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
      /Free
       Exec Sql Set Option Commit = *None;
       // Instead of creating WMPHP record, we will use it as the driver
       Exec sql
        declare curWMPHP cursor for
         select *
          from WMPHP
          where PHiDt = :xxDt and PHiPgm = 'W3060R';
       Exec sql open curWMPHP;
       Exec sql fetch curWMPHP for 999 rows into :dsWMPHP;
       Exec sql close curWMPHP;

       For i1 = 1 to 999;
          If dsWMPHP(i1).PHfnd = 0;
             Iter;
          Endif;
          xxCo# = dsWMPHP(i1).PHco#;
          xxFnd = dsWMPHP(i1).PHfnd;
          xxFyr = dsWMPHP(i1).PHfyr;
          xxChk#= dsWMPHP(i1).PHchk#;
          // Regenerate check requests
          Callp W3021R (%char(xxCo#):%char(xxFnd):qqDate:'CP');
          // Mark requests as EFT, where appropriate
          Callp W3030R1 (qqDate:%char(xxCo#):%char(xxFnd));
          // Get BUL2
          Exec sql
           select CObul2 into :xxBul2
            from SMCOP
            where COco# = :xxCo#
            limit 1;
          // insert check & batch numbers
          Setll R2key pr201;
          Dou %eof(WAR2L);
             Reade R2key pr201;
             Select;
             When %eof;
                Iter;
             When R2eftC = 'E';
                Iter;
             Endsl;
             xxBul2 += 1;
             R2bul2 = xxBul2;
             R2chck = xxChk#;
             Update pr201;
             xxChk# += 1;
          Enddo;
          // Update BUL2
          xxBul2 += 1;
          Exec sql
           update SMCOP
            set CObul2 = :xxBul2
            where COco# = :xxCo#;
      /End-free
     C*  This is the start of W3060R reproduction
     C     R2KEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  MOVE THE TRANSACTION RECORD TO THE AGENT COMMISSION RECORD
     C*
     C     ACKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '1'
     C     R2AMT         ANDNE     *ZERO
     C*
     C* Before writing payment record to agent commission file
     C*   retrieve greatest seq# for CO#/FND/AGT#/CHCK
     C                   Z-ADD     0             SVSEQ#
     C     ACKY2         SETGT     PAC21
     C     ACKY2         READPE    PAC21                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ACSEQ#        SVSEQ#
     C                   ENDIF
     C                   MOVE      R2CO#         ACCO#
     C                   MOVE      R2FND         ACFND
     C                   MOVE      XXFYR         ACFYR
     C                   MOVE      *ZERO         ACEMP#
     C                   MOVE      *ZERO         ACDIV
     C                   MOVE      R2AGT#        ACAGT#
     C                   MOVE      *ZERO         ACPROD
     C                   MOVE      XXDT          ACPODT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACTRDT
     C*
     C                   MOVE      *ZERO         ACTOCL
     C                   MOVE      *ZERO         ACRATE
     C                   MOVE      R2AMT         ACAMT
      /free
       If R2eftc = 'E';
          ACchck = GetNextEFT#(R2co#:R2acct);
       Else;
          ACchck = R2chck;
       Endif;
      /end-free
     C                   MOVE      R2COCD        ACCOCD
     C                   MOVE      R2COTP        ACCOTP
     C                   MOVE      R2DESC        ACDESC
     C                   MOVE      R2CKTP        ACCKTP
      /Free
       ACeftc = R2eftc;
      /End-Free
     C                   MOVE      R2ACCT        ACACCT
     C                   MOVE      *BLANK        ACSTAT
     C                   MOVE      R2USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C*
     C                   Z-ADD     DSTIME        ACTIME
     C                   Z-ADD     R2BUL2        ACBUL2
     C                   MOVE      Q1USER        ACIUSR
     C                   MOVE      Q1PGM         ACIPGM
     C                   Z-ADD     ACDT          ACIDT
     C                   Z-ADD     ACTIME        ACITM
     C*
     C                   WRITE     PAC01
     C*
     C* Read all records from WMACP for CO#/FND/AGT#
     C*   Update records where ACCOTP = 'C' and ACCCHK = *BLANKS
     C     ACKY3         CHAIN     PAC21                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ACCOTP        IFEQ      'C'
     C     ACCCHK        ANDEQ     *BLANK
     C                   Z-ADD     R2CHCK        ACCHCK
     C                   Z-ADD     R2AMT         ACCKAM
     C                   ADD       1             SVSEQ#
     C                   Z-ADD     SVSEQ#        ACSEQ#
     C                   MOVE      'Y'           ACCCHK
     C*
     C                   MOVE      R2USER        ACUSER
     C                   MOVE      Q1PGM         ACPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   Z-ADD     DSTIME        ACTIME
     C*
     C                   Z-ADD     ACDT          ACCKDT
     C*
     C                   UPDATE    PAC21
     C                   ENDIF                                                  ACCOTP = 'C'
     C*
     C     ACKY3         READE     PAC21                                  60
     C                   ENDDO
     C                   END
     C*
      * Bank Info check # doesn't need to be updated.
     C*
     C*  UPDATE THE TRANSACTION AS PROCESSED
     C*
     C                   MOVE      'Y'           R2STAT
     C                   UPDATE    PR201
     C*
     C     R2KEY         READE     PR201                                  60
     C                   END
      /Free
          // Produce check register;
          Callp W3070C (%char(xxCo#):qqDate:%char(xxFnd):'QPRINT':' ');
          // Clear Requests;
          Callp W3080R (qqDate:%char(xxCo#):%char(xxFnd));
       Endfor;
       *inlr = *on;
      /End-free
