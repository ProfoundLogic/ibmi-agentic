     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8022R - SPREAD NET DEDUCTIBLE RECOVERIES
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM PRESENTS A LIST OF DEDUCTIBLE BILLED         *
     H*     AMOUNTS FOR THE EMPLOYER SELECTED IN W8021R.  IT WILL     *
     H*     AUTOMATICALLY SPREAD THE CHECK AMOUNT ACROSS THE BILLED   *
     H*     AMOUNTS TO BE POSTED AS RECOVERIES.  THE USER MAY CHANGE  *
     H*     THE AMOUNTS TO BE SPREAD ACROSS OTHER BILLINGS, PRIOR TO  *
     H*     SUBMITTING THE JOB TO POST THE RECOVERIES.                *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     W8021R - POST NET RECOVERIES PENDING                      *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     W8021C - UPDATE NET RECOVERIES PENDING                    *
     H*                                                               *
     H*   PROGRAMMER: C:24                                            *
     H*   CREATED:    03/01/95    PROJECT:WC311                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/28/18 C:079 Project: WC2654                                *
     H*                Exclude records that are not in the group year *
     H*                                                               *
     H* 12/09/15 C:136 Project: WC2393                                *
     H*                Change sort order so that negative billed      *
     H*                amounts are spread to first by default so      *
     H*                that recoveries can be spread properly         *
     H*                                                               *
     H* 02/01/99 C:15 Support: WC065/15291                            *
     H*               Chgd pgm to be able to process negative pmts.   *
     H*                                                               *
     H* 04/09/98 C:65 PROJECT: WC812                                  *
     H*               CHANGED AMOUNT FIELD TO REMAINING AMOUNT AND    *
     H*               REMOVED EDIT TO ALLOW A PARTIAL PAYMENT.        *
     H*                                                               *
     H* 03/09/95 C:15 PROJECT: WC413                                  *
     H*               ADDED AN EDIT TO THE 'PENDING RECOVERY' AMOUNT  *
     H*               SO THE 'AMOUNT RECOVERED' WILL NOT EXCEED THE   *
     H*               'AMOUNT BILLED'.                                *
     H*                                                               *
     H* 09/16/96 C:33 PROJECT: WC585                                  *
     H*               REMOVED EDIT WHICH PREVENTS THE USER FROM       *
     H*               APPLYING PART OF A DED RECOVERY CHECK.          *
     H*               W8021R1 HAD TO BE MODIFIED TO ACCOMMODATE THIS  *
     H*               REVISION.                                       *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   04  -  THE SCREEN WAS CHANGED                               *
     H*   31-40 - IDENTIFY WHICH AMOUNTS CHANGED ON SCREEN            *
     H*   41-50 - CONTROL DISPLAY OF INPUT AMOUNTS                    *
     H*   71-80 - INVALID PENDING RECOVERY AMOUNTS                    *
     H*   60  -  GENERAL WORK INDICATOR                               *
     H*****************************************************************
     FW8022F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWADDL     UF A E           K DISK
     FWDDDL5    IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWADDL1    UF   E           K DISK
     F                                     RENAME(PDD01:PDD03)
     FWADQL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     D SO              S              3  0 DIM(10)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S             19    DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FY                     1     30  0
     D                                     DIM(10)
     D  SCFY01                 1      3  0
     D  SCFY02                 4      6  0
     D  SCFY03                 7      9  0
     D  SCFY04                10     12  0
     D  SCFY05                13     15  0
     D  SCFY06                16     18  0
     D  SCFY07                19     21  0
     D  SCFY08                22     24  0
     D  SCFY09                25     27  0
     D  SCFY10                28     30  0
     D                 DS
     D  CA                     1     70  0
     D                                     DIM(10)
     D  SCCA01                 1      7  0
     D  SCCA02                 8     14  0
     D  SCCA03                15     21  0
     D  SCCA04                22     28  0
     D  SCCA05                29     35  0
     D  SCCA06                36     42  0
     D  SCCA07                43     49  0
     D  SCCA08                50     56  0
     D  SCCA09                57     63  0
     D  SCCA10                64     70  0
     D                 DS
     D  PO                     1    080  0
     D                                     DIM(10)
     D  SCPO01               001    008  0
     D  SCPO02               009    016  0
     D  SCPO03               017    024  0
     D  SCPO04               025    032  0
     D  SCPO05               033    040  0
     D  SCPO06               041    048  0
     D  SCPO07               049    056  0
     D  SCPO08               057    064  0
     D  SCPO09               065    072  0
     D  SCPO10               073    080  0
     D                 DS
     D  DC                     1     20
     D                                     DIM(10)
     D  SCDC01                 1      2
     D  SCDC02                 3      4
     D  SCDC03                 5      6
     D  SCDC04                 7      8
     D  SCDC05                 9     10
     D  SCDC06                11     12
     D  SCDC07                13     14
     D  SCDC08                15     16
     D  SCDC09                17     18
     D  SCDC10                19     20
     D                 DS
     D  AM                     1     90  2
     D                                     DIM(10)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  SCAR01                 1      9  2
     D  SCAR02                10     18  2
     D  SCAR03                19     27  2
     D  SCAR04                28     36  2
     D  SCAR05                37     45  2
     D  SCAR06                46     54  2
     D  SCAR07                55     63  2
     D  SCAR08                64     72  2
     D  SCAR09                73     81  2
     D  SCAR10                82     90  2
     D                 DS
     D  PD                     1     90  2
     D                                     DIM(10)
     D  SCPD01                 1      9  2
     D  SCPD02                10     18  2
     D  SCPD03                19     27  2
     D  SCPD04                28     36  2
     D  SCPD05                37     45  2
     D  SCPD06                46     54  2
     D  SCPD07                55     63  2
     D  SCPD08                64     72  2
     D  SCPD09                73     81  2
     D  SCPD10                82     90  2
      *** beg chg ***** 12/09/15 ************************
     D***********      DS
     D**XXKEY****              1     20
     D**XXBFYR***              1      3  0
     D**XXPODT***              4     10  0
     D**XXCASE***             11     17  0
     D**XXSORT***             18     20  0
     D                 DS
     D  XXKEY                  1     29
     D  XXAMTB                 1      9  2
     D  XXBFYR                10     12  0
     D  XXPODT                13     19  0
     D  XXCASE                20     26  0
     D  XXSORT                27     29  0
      *** end chg ***** 12/09/15 ************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C*  DEDUCTIBLE PAYMENT RECOVERIES PENDING FILE
     C*
     C     DQKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCHK#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DEDUCTIBLE DETAIL BILLS FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *** beg add ***** 12/09/15 **************************
     C                   KFLD                    XXAMTB
      *** end add ***** 12/09/15 **************************
     C                   KFLD                    XXBFYR
     C                   KFLD                    XXPODT
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSORT
     C*
     C     DDKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     *LIKE         DEFINE    SCAMT         XXAMT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCFYR
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCCHK#
     C                   PARM                    SCAMT
     C                   PARM                    QQRTCD            1
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        SCENAM
     C*
     C*   VERIFY THAT THERE IS NO WORK CURRENTLY BEING DONE
     C*   FOR THIS EMPLOYER
     C*
     C     EMKY          CHAIN(N)  PDQ01                              60
     C     *IN60         DOWEQ     *OFF
     C     DQSTAT        IFNE      *BLANK
     C                   MOVE      'E'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C     EMKY          READE(N)  PDQ01                                  60
     C                   ENDDO
     C*
     C*   UPDATE THE WORK IN PROGRESS STATUS FOR THIS CHECK
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C                   MOVE      'S'           DQSTAT
     C  N60              UPDATE    PDQ01
       //*** Beg Add *** 12/28/18 *****************************************
       // Delete records for this employer/division
      /Free
                         Setll (UPco#:scFnd:scEmp#:scDiv) pdd01;
                         Reade (UPco#:scFnd:scEmp#:scDiv) pdd01;
                         Dow Not %Eof(waddl);
                            Delete pdd01;
                            Reade (UPco#:scFnd:scEmp#:scDiv) pdd01;
                         Enddo;
      /End-Free
       //*** End Add *** 12/28/18 *****************************************
     C*
     C*   IF NO OVERRIDE RECORDS EXIST, LOAD THEM NOW
     C*
     C     DDKY2         CHAIN(N)  PDD01                              60
     C     *IN60         IFEQ      *ON
     C                   EXSR      SRLOAD
     C*
     C*  OVERRIDE RECORDS EXIST, APPLY THE DEFAULT SPREAD
     C*
     C                   ELSE
     C                   EXSR      SPREAD
     C                   ENDIF
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = Exit
     C*
     C     *INKC         IFEQ      '1'
     C*
     C*   UPDATE THE WORK IN PROGRESS STATUS FOR THIS CHECK
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C                   MOVE      ' '           DQSTAT
     C  N60              UPDATE    PDQ01
     C*
     C                   MOVE      'A'           QQRTCD
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   EXSR      DLTOVR
     C*** END ADD *****  04/09/98  ************************************
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = Cancel
     C*
     C     *INKL         IFEQ      '1'
     C*
     C*   UPDATE THE WORK IN PROGRESS STATUS FOR THIS CHECK
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C                   MOVE      ' '           DQSTAT
     C  N60              UPDATE    PDQ01
     C*
     C                   MOVE      'B'           QQRTCD
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   EXSR      DLTOVR
     C*** END ADD *****  04/09/98  ************************************
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ANY CHANGES WERE MADE ON THE SCREEN, UPDATE THE OVERRIDE
     C*  FILE AND ADJUST THE BALANCE.
     C*
     C     *IN04         IFEQ      *ON
     C     31            DO        40            I1
     C*
     C     *IN(I1)       IFEQ      *ON
     C     I1            SUB       30            X                 3 0
     C*
     C*  IF THE AMOUNT RECOVERED + THE PENDING RECOVERY AMOUNT IS
     C*  GREATER THAN THE AMOUNT BILLED, SEND AN ERROR MSG.
     C*
     C     AR(X)         ADD       PD(X)         XXWK92            9 2
     C*
     C     AM(X)         IFGT      0
     C     XXWK92        ANDGT     AM(X)
     C     X             ADD       70            Y                 3 0
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C     AM(X)         IFLT      0
     C     XXWK92        ANDLT     AM(X)
     C     X             ADD       70            Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      FY(X)         XXBFYR
     C                   Z-ADD     PO(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   MOVE      CA(X)         XXCASE
     C                   MOVE      SO(X)         XXSORT
      *** beg add ***** 12/09/15 *******************************
     C                   MOVE      AM(X)         XXAMTB
      *** end add ***** 12/09/15 *******************************
     C     DDKY          CHAIN     PDD01                              60
     C                   SUB       DDAMTP        SCBAL
     C                   ADD       PD(X)         SCBAL
     C                   Z-ADD     PD(X)         DDAMTP
     C  N60              UPDATE    PDD01
     C                   MOVE      *OFF          *IN(I1)
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXKEY
     C     DDKY          CHAIN(N)  PDD01                              60
     C     DDKY2         READE(N)  PDD01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXKEY
     C     DDKY          CHAIN(N)  PDD01                              60
     C                   READP(N)  PDD01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCBFYR        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *ZERO         KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      SCBFYR        XXBFYR
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   MOVE      *ZERO         XXCASE
     C                   MOVE      *ZERO         XXSORT
     C                   MOVE      *ZERO         SCBFYR
     C                   MOVE      *ZERO         SCPODT
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      *LOVAL        XXAMTB
      *** end add ***** 12/09/15 ******************************
     C     DDKY          SETLL     PDD01
     C     *IN21         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   ELSE
     C     DDKY2         READPE(N) PDD01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   ELSE
     C                   MOVE      *BLANK        XXDOWN
     C                   ENDIF
     C     DDKY          SETLL     PDD01
     C                   ENDIF
     C                   MOVE      '0'           *IN21
     C     DDKY2         READE(N)  PDD01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF SOMETHING CHANGED, REDISPLAY THE SCREEN
     C*
     C     *IN04         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C*
     C*  IF NO CHANGES WERE MADE, CHECK THE BALANCE.  IF IT IS
     C*  ZERO, SUBMIT THE RECOVERY AND EXIT THE PROGRAM.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCCHK#        QQCHK#            7
     C                   CALL      'W8021C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCHK#
     C*
     C                   MOVE      '*'           QQRTCD
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C     41            DO        50            I1
     C                   MOVE      *OFF          *IN(I1)
     C                   ENDDO
     C                   MOVE      *ZEROS        FY
     C                   MOVE      *ZEROS        CA
     C                   MOVE      *ZEROS        PO
     C                   MOVE      *BLANKS       DC
     C                   MOVE      *ZEROS        AM
     C                   MOVE      *ZEROS        AR
     C                   MOVE      *ZEROS        PD
     C                   MOVE      *ZEROS        SO
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      DDBFYR        FY(I1)
     C                   MOVE      DDCASE        CA(I1)
     C*
     C                   Z-ADD     DDPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PO(I1)
     C*
     C                   MOVE      DDRECD        DC(I1)
     C                   MOVE      DDAMTB        AM(I1)
     C                   MOVE      DDAMTR        AR(I1)
     C                   MOVE      DDAMTP        PD(I1)
     C                   MOVE      DDSORT        SO(I1)
     C                   ADD       40            I1
     C                   MOVE      *ON           *IN(I1)
     C                   SUB       40            I1
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     DDKY2         READE(N)  PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      FY(1)         XXBFYR
     C*
     C                   Z-ADD     PO(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      AM(1)         XXAMTB
      *** end add ***** 12/09/15 ******************************
     C                   MOVE      CA(1)         XXCASE
     C                   MOVE      SO(1)         XXSORT
     C                   MOVE      XXKEY         KEY(1)
     C                   MOVE      FY(10)        XXBFYR
     C*
     C                   Z-ADD     PO(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      AM(10)        XXAMTB
      *** end add ***** 12/09/15 ******************************
     C                   MOVE      CA(10)        XXCASE
     C                   MOVE      SO(10)        XXSORT
     C                   MOVE      XXKEY         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     11            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     DDKY2         READPE(N) PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      FY(1)         XXBFYR
     C*
     C                   Z-ADD     PO(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      AM(1)         XXAMTB
      *** end add ***** 12/09/15 ******************************
     C                   MOVE      CA(1)         XXCASE
     C                   MOVE      SO(1)         XXSORT
     C                   MOVE      XXKEY         KEY(1)
     C                   MOVE      FY(10)        XXBFYR
     C*
     C                   Z-ADD     PO(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      AM(10)        XXAMTB
      *** end add ***** 12/09/15 ******************************
     C                   MOVE      CA(10)        XXCASE
     C                   MOVE      SO(10)        XXSORT
     C                   MOVE      XXKEY         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZERO         XXBFYR
     C                   MOVE      *ZERO         XXPODT
     C                   MOVE      *ZERO         XXCASE
     C                   MOVE      *ZERO         XXSORT
      *** beg add ***** 12/09/15 ******************************
     C                   MOVE      *LOVAL        XXAMTB
      *** end add ***** 12/09/15 ******************************
     C     DDKY          SETLL     PDD01
     C     DDKY2         READE(N)  PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     DDKY2         READE(N)  PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*================================================================
     C*
     C*  SUBROUTINE TO COPY DEDUCTIBLE BILLED RECORDS INTO OVERRIDE FILE
     C*  AND SPREAD PENDING RECOVERIES.
     C*
     C     SRLOAD        BEGSR
     C*
     C                   Z-ADD     SCAMT         XXAMT
     C     DDKY2         CHAIN     PDD02                              60
     C     *IN60         DOWEQ     *OFF
       //*** Beg Add *** 12/28/18 *****************************************
       // Exclude if not in the group year
                         If DDbFyr = scFyr;
       //*** End Add *** 12/28/18 *****************************************
     C*
     C*  IF AN AMOUNT REMAINS TO BE SPREAD, COMPARE IT TO THE
     C*  PENDING RECOVERY AMOUNT AND APPLY THE LESSER AMOUNT TO
     C*  THE TRANSACTION RECORD.
     C*
     C     XXAMT         IFNE      *ZERO
     C*
     C*** BEGIN ADD *** 02/01/99 **************************************
     C     XXAMT         IFLT      *ZERO
     C*
     C     DDAMTP        IFLT      XXAMT
     C                   Z-ADD     XXAMT         DDAMTP
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD ***** 02/01/99 **************************************
     C*
     C     DDAMTP        IFGT      XXAMT
     C                   Z-ADD     XXAMT         DDAMTP
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/01/99 **************************************
     C                   ENDIF
     C*** END ADD ***** 02/01/99 **************************************
     C*
     C                   SUB       DDAMTP        XXAMT
     C*
     C                   ELSE
     C                   MOVE      *ZERO         DDAMTP
     C                   ENDIF
     C*
     C                   WRITE     PDD01
       //*** Beg Add *** 12/28/18 *****************************************
                         Endif;
       //*** End Add *** 12/28/18 *****************************************
     C     DDKY2         READE     PDD02                                  60
     C*
     C                   ENDDO
     C                   Z-ADD     XXAMT         SCBAL
     C*
     C                   ENDSR
     C*================================================================
     C*
     C*  SUBROUTINE TO SPREAD PENDING RECOVERIES ACROSS OVERRIDE
     C*  FILE RECORDS.
     C*
     C     SPREAD        BEGSR
     C*
     C*  FIRST, SET ANY LEFTOVER PENDING FIELDS TO *ZERO
     C*
     C     DDKY2         CHAIN     PDD03                              60
     C*
     C     *IN60         DOWEQ     *OFF
       //*** Beg Add *** 12/28/18 *****************************************
       // Exclude if not in the group year
                         If DDbFyr = scFyr;
       //*** End Add *** 12/28/18 *****************************************
     C                   MOVE      *ZERO         DDAMTP
     C                   UPDATE    PDD03
       //*** Beg Add *** 12/28/18 *****************************************
                         Endif;
       //*** End Add *** 12/28/18 *****************************************
     C     DDKY2         READE     PDD03                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     SCAMT         XXAMT
     C     DDKY2         CHAIN     PDD01                              60
     C*
     C     *IN60         DOWEQ     *OFF
       //*** Beg Add *** 12/28/18 *****************************************
       // Exclude if not in the group year
                         If DDbFyr = scFyr;
       //*** End Add *** 12/28/18 *****************************************
     C*
     C*  COMPARE THE AMOUNT REMAINING TO BE SPREAD TO THE PENDING
     C*  RECOVERY AMOUNT AND APPLY THE LESSER AMOUNT TO THE
     C*  TRANSACTION RECORD.
     C*
     C*
     C     DDAMTB        SUB       DDAMTR        DDAMTP
     C*
     C*** BEGIN ADD *** 02/01/99 **************************************
     C     XXAMT         IFLT      *ZERO
     C*
     C     DDAMTP        IFLT      XXAMT
     C                   Z-ADD     XXAMT         DDAMTP
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD ***** 02/01/99 **************************************
     C*
     C     DDAMTP        IFGT      XXAMT
     C                   Z-ADD     XXAMT         DDAMTP
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/01/99 **************************************
     C                   ENDIF
     C*** END ADD ***** 02/01/99 **************************************
     C*
     C                   SUB       DDAMTP        XXAMT                    60
     C*
     C                   UPDATE    PDD01
       //*** Beg Add *** 12/28/18 *****************************************
                         Endif;
       //*** End Add *** 12/28/18 *****************************************
     C  N60DDKY2         READE     PDD01                                  60
     C*
     C                   ENDDO
     C                   Z-ADD     XXAMT         SCBAL
     C*
     C                   ENDSR
     C*================================================================
     C*** BEGIN ADD ***  04/09/98  ************************************
     C     DLTOVR        BEGSR
     C*
     C     DDKY2         CHAIN     PDD01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PDD01
     C     DDKY2         READE     PDD01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*** END ADD *****  04/09/98  ************************************
**
ROLL UP
ROLL DOWN
