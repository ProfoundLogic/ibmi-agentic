     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H/TITLE W0077R02 - MT Employer Contacts
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0077R                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED:   01/31/02 C:61  PROJECT WC1070                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     A*                                                               *
     A* 07/13/12  C:282 CI067                                         *
     A*                 Add new flag to denote "SR Email Notification"*
     A*                 Contact.                                      *
     H*                                                               *
     H* 08/11/09  C:190 SY184                                         *
     H*                 Changes Preferred Billing Method to be a      *
     H*                 client flagged field for client 132.          *
     H*                                                               *
     H* 02/19/09 C:189 PROJECT: 93WHL PRISS#9                         *
     H*                Changed the INDICATOR usage due to which       *
     H*                screen is getting locked                       *
     H*                                                               *
     H* 01/07/09 C:197 PROJECT: 93WHL PRISS#9                         *
     H*                                                               *
     H* 10/01/08 C:190 PROJECT: 132WLB                                *
     H*                Changed the Employer Contact screen to allow   *
     H*                the  user to designate Preferred Billing       *
     H*                method.                                        *
     H*                                                               *
     H* 08/22/08 C:197 PROJECT: 93WHL                                 *
     H*               -CLIENT SPECIFIC - IDENTITY CONTACTS FOR INSURED*
     H*                                                               *
     H* 11/06/06 C:136 PROJECT: 71WHE (ADDITIONAL CHANGES)            *
     H*               -CLIENT SPECIFIC - ADD 4TH CONTACT.             *
     H*                                                               *
     H* 12/08/03 C:61  SUPPORT WA099 32541                            *
     H*                Display 'Not on proper field for Search'       *
     H*                message.                                       *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  71 - 78  Error messages                                      *
     H*  81       'Not proper field for search'                       *
     H*  93       Duplicate Record                                    *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0077F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Employer Contacts
     FWMECP     UF A E             DISK
     F                                     RENAME(PEC01:DBRCDF)
     FWMECL1    IF   E           K DISK
     F                                     INFDS(ECFDS)
     FWMECL     IF   E           K DISK
     F                                     INFDS(EC2FDS)
     F                                     RENAME(PEC01:PEC02)
      *
      *  Employer Master
     FWMEMP     IF   E             DISK
      *
      *  Group Master
     FWMFNL     IF   E           K DISK
      *
      *  Admin/Darts Control
     FWMEXL     IF   E           K DISK
      *
      *  System State
     FSTSTL3    IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     F*  SYSTEM TABLE FILE
     F*
     FWTGTL     IF   E           K DISK
     F*
      *** BEG ADD *** 07/13/12 *************************************************
      *  Workers Compensation Control File
     fssylf     IF   E           K DISK
      *** END ADD *** 07/13/12 *************************************************
      *
      *
      *
     D ECFDS           DS
     D  ECRRN                397    400B 0
      *
     D EC2FDS          DS
     D  EC2RRN               397    400B 0
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      *
      ***** Beg Del ***** 07/13/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 07/13/12 ***************************************
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Employer Contacts')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      ***** Beg Add ***** 07/13/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 07/13/12 ***************************************
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WMECP
     C                   PARM                    @@RRN1            9 0          WMEMP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  System State Code Table
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
      *  Employer Contacts
      *
     C     ECKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCPCNT
      *
     C     ECKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCLSN
     C                   KFLD                    SCFSN
     C                   KFLD                    SCMID
     C                   KFLD                    SCPOSN
     C                   KFLD                    SCPCNT
      *
      *
     C     ECKY3         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCCCDE
      *
     C     ECKY4         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCLSN
     C                   KFLD                    SCFSN
     C                   KFLD                    SCMID
     C                   KFLD                    SCPOSN
     C                   KFLD                    SCCCDE
      *
      *  Group Master
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  Admin/Darts control
      *
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
     C*  SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      *
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
      *
      *******Beg Del********07/13/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********07/13/12**********************************
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * Last Name is required entry
      *
     C  N90SCLSN         IFEQ      *BLANKS
     C                   SETON                                        90  71
     C                   ENDIF
      *
      * Either Title or First Name must be entered.
      *
     C  N90SCTITL        IFEQ      *BLANKS
     C     SCFSN         ANDEQ     *BLANKS
     C                   SETON                                        90  72
     C                   ENDIF
      *
      * State must be valid if entered
      *
     C  N90SCST          IFNE      *BLANK
     C                   MOVEL     SCST          KKST
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        90  78
     C                   ENDIF
      *
      *
      * Preferred Billing method validation
      *
     C  N90SCPEF         IFEQ      *BLANKS
     C     CLTID         ANDEQ     '132'
     C                   SETON                                        90  56
     C                   ENDIF                                                  SCPEF <> ' '
      *
     C  N90SCPEF         IFNE      *BLANKS
     C     SCPEF         ANDNE     'P'
     C     SCPEF         ANDNE     'E'
     C     SCPEF         ANDNE     'F'
     C                   SETON                                        90  58
     C                   ENDIF                                                  SCPEF<>'P/E/F.
     C     *IN53         IFEQ      *OFF
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C  N90SCPCNT        IFNE      *BLANK
     C     SCPCNT        ANDNE     '1'
     C     SCPCNT        ANDNE     '2'
     C     SCPCNT        ANDNE     '3'
     C     SCPCNT        ANDNE     '4'
     C                   SETON                                        90  73
     C                   ENDIF
     C                   OTHER
     C  N90SCPCNT        IFNE      *BLANK
     C     SCPCNT        ANDNE     '1'
     C     SCPCNT        ANDNE     '2'
     C     SCPCNT        ANDNE     '3'
     C                   SETON                                        90  73
     C                   ENDIF
     C                   ENDSL
     C                   ELSE                                                   *IN53 IFEQ *OFF
     C*
     C  N90SCCCDE        IFNE      *BLANK
     C                   MOVEL(P)  'EMCT'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCCDE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        5590
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCCDES
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                  SCCCDE <> ' '
     C*
     C                   ENDIF                                                  *IN53 = OFF
     C*
      *
      * No other record for this employer may have the same
      * Primary Contact designation
      *
     C  N90SCPCNT        IFNE      *BLANK
     C     SCCCDE        ORNE      *BLANK
      *
     C     @@MODE        IFEQ      'A'
     C  N53ECKY1         CHAIN     PEC01                              60
     C   53ECKY3         CHAIN     PEC01                              60
     C  N60              SETON                                        90  74
     C                   ENDIF                                                  @@MODE = A
      *
     C     @@MODE        IFEQ      'C'
     C  N53ECKY1         CHAIN     PEC01                              60
     C   53ECKY3         CHAIN     PEC01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ECRRN         IFNE      @@RRN
     C                   SETON                                        90  74
     C                   LEAVE
     C                   ENDIF
      *
     C  N53ECKY1         READE     PEC01                                  60
     C   53ECKY1         READE     PEC01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  @@MODE = C
      *
     C                   ENDIF                                                  PCNT
      *
      * If Phone Number, Cell Number and Primary Contact entered THEN
      * Primary Phone Flag must be entered.
      *
     C  N90SCPCNT        IFNE      *BLANK
     C     SCPH1         ANDNE     *ZERO
     C     SCCPH1        ANDNE     *ZERO
     C     SCPPHF        ANDEQ     *BLANK
     C                   SETON                                        90  75
     C                   ENDIF
      *
      * Primary Phone Flag must be valid if entered
      *
     C  N90SCPPHF        IFNE      *BLANK
     C     SCPPHF        ANDNE     'P'
     C     SCPPHF        ANDNE     'C'
     C                   SETON                                        90  76
     C                   ENDIF
      *
      *** BEG ADD ***** 07/13/12 *************************************
      /free
       // SR Email Notification only be entered only when Email address is
       // provided.

       If WCFL48 = 'Y';

          If *In90 = *off and (SCsrem <> *blanks and SCsrem <> 'Y');
             Eval *In90 = *on;
             Eval *In61 = *on;
          EndIf;

          If *In90 = *off and SCemal = *blanks and SCsrem <> *blanks;
             Eval *In90 = *on;
             Eval *In62 = *on;
          EndIf;

       EndIf;
      /end-free
      *** END ADD ***** 07/13/12 *************************************
      * Record must have a unique key
      *    co#/fnd/emp#/div/lsn/fsn/mid/posn/pcnt
      *
     C     *IN90         IFEQ      *OFF
      *
     C     @@MODE        IFEQ      'A'
     C  N53ECKY2         CHAIN     PEC02                              60
     C   53ECKY4         CHAIN     PEC02                              60
     C  N60              SETON                                        90  77
     C                   ENDIF                                                  @@MODE = A
      *
     C     @@MODE        IFEQ      'C'
     C  N53ECKY2         CHAIN     PEC02                              60
     C   53ECKY4         CHAIN     PEC02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EC2RRN        IFNE      @@RRN
     C                   SETON                                        90  77
     C                   LEAVE
     C                   ENDIF
      *
     C  N53ECKY2         READE     PEC02                                  60
     C   53ECKY4         READE     PEC02                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  @@MODE = C
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90ECDT          IFGT      SAVEDT
     C     ECDT          OREQ      SAVEDT
     C     ECTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  EMDT>SAVEDT
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      * ZZZ
      *** Beg Add ***** 07/13/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 07/13/12 **************************************
      *================================================================
      * KJDO - F10=Search
      *================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * State
      *
     C     HFLD          WHENEQ    'SCST'
      *
     C                   CALL      'SY601R'
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      *ZEROS        #1RRN             9 0
      *
     C     #1RRN         IFEQ      *ZERO
     C                   SETON                                        79
      *
     C                   ELSE
      *
     C     #1RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        80
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCCCDE'
      *
     C                   MOVEL     'EMCT'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
      *
     C                   CALL      'W2493R'
     C                   PARM      '*BLANKS'     ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM      '*BLANKS'     QQRTC             1
      *
     C                   MOVEL(P)  *BLANKS       SCCDES
      *
     C     XXKEY         IFNE      *BLANKS
      *
     C     GTKY          CHAIN     PGT01                              99
      *
     C  N99              MOVEL(P)  GTDESC        SCCDES
     C*
     C                   MOVE      *ON           *IN33
     C                   MOVEL     XXKEY         SCCCDE
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN32
     C*
     C                   ENDIF
      *
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  ECTITL        SCTITL
     C                   MOVEL(P)  ECFSN         SCFSN
     C                   MOVEL(P)  ECMID         SCMID
     C                   MOVEL(P)  ECLSN         SCLSN
     C                   MOVEL(P)  ECPOSN        SCPOSN
     C                   MOVEL(P)  ECADR1        SCADR1
     C                   MOVEL(P)  ECADR2        SCADR2
     C                   MOVEL(P)  ECCTY         SCCTY
     C                   MOVEL(P)  ECST          SCST
      *
     C                   MOVEL     ECZIP         SCZIP5
     C                   MOVE      ECZIP         SCZIP4
      *
     C                   Z-ADD     ECAREA        SCAREA
     C                   MOVEL     ECPHN#        SCPH1
     C                   MOVE      ECPHN#        SCPH2
     C                   Z-ADD     ECEXT#        SCEXT#
      *
     C                   Z-ADD     ECFARA        SCFARA
     C                   MOVEL     ECFPHN        SCFPH1
     C                   MOVE      ECFPHN        SCFPH2
      *
     C                   Z-ADD     ECCARA        SCCARA
     C                   MOVEL     ECCPHN        SCCPH1
     C                   MOVE      ECCPHN        SCCPH2
      *
     C                   MOVE      ECPPHF        SCPPHF
     C                   MOVEL(P)  ECEMAL        SCEMAL
     C*** BEG ADD *** 07/13/12 *************************************************
      /free
       SCsrem = ECsrem;
      /end-free
     C*** END ADD *** 07/13/12 *************************************************
     C                   MOVE      *BLANKS       SCCDES
     C     *IN53         IFEQ      *OFF
     C                   MOVE      ECPCNT        SCPCNT
     C                   ELSE
     C                   MOVE      ECPCNT        SCCCDE
     C                   MOVE(P)   'EMCT'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCCDE        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C  N60              MOVEL(P)  GTDESC        SCCDES
     C*
     C                   ENDIF
     C                   MOVEL     ECPEF         SCPEF
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     EMCO#         ECCO#
     C                   Z-ADD     EMFND         ECFND
     C                   Z-ADD     EMEMP#        ECEMP#
     C                   Z-ADD     EMDIV         ECDIV
     C                   MOVEL(P)  SCTITL        ECTITL
     C                   MOVEL(P)  SCFSN         ECFSN
     C                   MOVEL(P)  SCMID         ECMID
     C                   MOVEL(P)  SCLSN         ECLSN
     C                   MOVEL(P)  SCPOSN        ECPOSN
     C                   MOVEL(P)  SCADR1        ECADR1
     C                   MOVEL(P)  SCADR2        ECADR2
     C                   MOVEL(P)  SCCTY         ECCTY
     C                   MOVEL(P)  SCST          ECST
     C     SCZIP5        MULT      10000         ECZIP
     C                   ADD       SCZIP4        ECZIP
     C                   Z-ADD     SCAREA        ECAREA
     C     SCPH1         MULT      10000         ECPHN#
     C                   ADD       SCPH2         ECPHN#
     C                   Z-ADD     SCEXT#        ECEXT#
     C                   Z-ADD     SCFARA        ECFARA
     C     SCFPH1        MULT      10000         ECFPHN
     C                   ADD       SCFPH2        ECFPHN
     C                   Z-ADD     SCCARA        ECCARA
     C     SCCPH1        MULT      10000         ECCPHN
     C                   ADD       SCCPH2        ECCPHN
     C                   MOVE      SCPPHF        ECPPHF
     C                   MOVEL(P)  SCEMAL        ECEMAL
     C*** BEG ADD *** 07/13/12 *************************************************
      /free
       ECsrem = SCsrem;
      /end-free
     C*** END ADD *** 07/13/12 *************************************************
     C     *IN53         IFEQ      *OFF
     C                   MOVE      SCPCNT        ECPCNT
      *
     C                   ELSE
     C                   MOVE      SCCCDE        ECPCNT
     C                   ENDIF
     C*
     C                   MOVE      SCPEF         ECPEF
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     SEARCHES
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD         XC
      *
      * Get Header Information
      *
     C     @@RRN1        CHAIN     PEM01                              60
     C  N60              Z-ADD     EMFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C   60              MOVEL(P)  *BLANKS       SCNAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL(P)  ##EDN         SCEDN
      *
      * Get CX date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     ED(X)         XXEFDT
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
      *
     C     SRERR         COMP      *ON                                    20
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN54
     C                   ELSE
     C                   MOVE      *OFF          *IN54
     C                   ENDIF
      *
     C     CLTID         IFEQ      '093'
     C                   MOVE      *ON           *IN53
     C                   ELSE
     C                   MOVE      *OFF          *IN53
     C                   ENDIF
      *
      * Get Primary Contact Descriptive Prompts
      *
     C     EXKY          CHAIN     PEX01                              60
     C                   MOVEL(P)  EXCNT1        SCCNT1
     C                   MOVEL(P)  EXCNT2        SCCNT2
     C                   MOVEL(P)  EXCNT3        SCCNT3
     C   54              MOVEL(P)  EXCNT4        SCCNT4
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN57
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C*
     C     *IN09         IFEQ      *ON
     C                   MOVE      *ON           *IN57
     C                   ENDIF
     C*** BEG ADD *** 07/13/12 *************************************************
      /free
       // Check Flag WCFL48 S/R Payroll Online Phase 2&3 is enabled.
       Chain 'WC' ssylf;
       If %found(ssylf);
          If WCFL48 = 'Y';
             Eval *in91 = *On;
          Else;
             Eval *in91 = *Off;
          EndIf;
       EndIf;
      /end-free
     C*** END ADD *** 07/13/12 *************************************************
      *
     C                   ENDSR                                                  PGMSTR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        ECIUSR
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECIPGM
     C                   MOVEL     Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECIDT
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECITIM
     C                   MOVEL     DSTIME        ECTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMECP
     C                   ENDSR                                                  WRITER
      *
