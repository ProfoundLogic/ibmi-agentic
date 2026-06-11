     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE DATA AUTHORIZAION FILE.  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    S0009R - WORK WITH DATA RESTRICTIONS                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 03/05/99  PROGRAMMER: C:33  PROJECT: 48WOM/48MCB    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FS0009F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMAZP     UF A E             DISK
     F                                     RENAME(PAZ01:DBRCDF)
     FSMAZL     IF   E           K DISK
     F                                     RENAME(PAZ01:WAZ01)
     F                                     INFDS(WAZFDS)
     FSMAZL1    IF   E           K DISK
     F                                     RENAME(PAZ01:MAZ01)
     F                                     INFDS(MAZFDS)
     FSMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:WFN01)
     F                                     INFDS(WFNFDS)
     FFMSML     IF   E           K DISK    USROPN
     F                                     INFDS(SMFDS)
     FFMSMP     IF   E             DISK    USROPN
     F                                     RENAME(PSM01:PSM0P)
     FFMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:MFN01)
     F                                     INFDS(MFNFDS)
     FFMFNP     IF   E             DISK    USROPN
     F                                     RENAME(PFN01:MFN0P)
     FWMEMP     IF   E             DISK    USROPN
     FFMEMP     IF   E             DISK    USROPN
     F                                     RENAME(PEM01:MEM01)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D WAZFDS          DS
     D  WAZRRN               397    400B 0
     D MAZFDS          DS
     D  MAZRRN               397    400B 0
     D SMFDS           DS
     D  SMRRN                397    400B 0
     D WFNFDS          DS
     D  WFNRRN               397    400B 0
     D MFNFDS          DS
     D  MFNRRN               397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM1          C                   CONST('Data Restrictions')
     D WCMC            C                   CONST('WCMC/FSMS')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@APPL            1
     C                   PARM                    @@USR            10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     AZKYW         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    SCFND
     C*
     C     AZKYM         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C     FNKYW         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     FNKYM         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AZTEMP        XATEMP
     C     *LIKE         DEFINE    AZTEMP        XSTEMP
     C     *LIKE         DEFINE    WAZRRN        AZRRN
     C*
     C                   EXSR      LODPGT
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     AZSTAT        IFEQ      'I'
     C                   MOVE      'A'           AZSTAT
     C                   ELSE
     C                   MOVE      'I'           AZSTAT
     C                   ENDIF                                                  AZSTAT=I
     C*
     C                   MOVEL     Q1USER        AZUSER
     C                   MOVEL     Q1PGM         AZPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AZDT
     C*
     C                   MOVEL     DSTIME        AZTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   CLOSE     WMFNL
     C     @@APPL        WHENEQ    'M'
     C                   CLOSE     FMSML
     C                   CLOSE     FMFNL
     C                   ENDSL
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C   30
     CANN30              READ      WAZ01                                  60
     C   30
     CANN30              READ      MAZ01                                  60
     C*
     C                   SELECT
     C*
     C     @@APPL        WHENEQ    'W'
     C*
     C*  VALIDATE GROUP
     C*
     C  N90FNKYW         CHAIN     WFN01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ELSE
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C     @@APPL        WHENEQ    'M'
     C*
     C*  VALIDATE SUBSCRIBER
     C*
     C  N90SMKY          CHAIN     PSM01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ELSE
     C                   MOVEL(P)  SMNAME        SCSNAM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  VALIDATE GROUP, IF ENTERED
     C*
     C  N90SCFND         IFNE      *ZEROS
     C*
     C     FNKYM         CHAIN     MFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ELSE
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCFND<>*ZEROS
     C*
     C                   ENDSL
     C*
     C*  VERIFY EMPLOYER RANGE IS IN ASCENDING ORDER
     C*
     C  N90SCFEMP        IFGT      SCTEMP
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCFEMP>SCTEMP
     C*
     C*  EMPLOYER RANGE CAN NOT OVERLAP ANOTHER RANGE
     C*
     C  N90              SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKYW         CHAIN     WAZ01                              60
     C                   Z-ADD     WAZRRN        AZRRN
     C     @@APPL        WHENEQ    'M'
     C     AZKYM         CHAIN     MAZ01                              60
     C                   Z-ADD     MAZRRN        AZRRN
     C                   ENDSL
     C*
     C                   Z-ADD     SCTEMP        XSTEMP
     C*
     C     SCTEMP        IFEQ      *ZERO
     C                   MOVE      *HIVAL        XSTEMP
     C                   ENDIF                                                  SCTEMP=*ZERO
     C*
     C  N90*IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     AZTEMP        XATEMP
     C*
     C     AZTEMP        IFEQ      *ZERO
     C                   MOVE      *HIVAL        XATEMP
     C                   ENDIF                                                  AZTEMP=*ZERO
     C*
     C     XSTEMP        IFLT      AZFEMP
     C     SCFEMP        ORGT      XATEMP
     C     AZSTAT        OREQ      'I'
     C     AZRRN         OREQ      @@RRN
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKYW         READE     WAZ01                                  60
     C                   Z-ADD     WAZRRN        AZRRN
     C     @@APPL        WHENEQ    'M'
     C     AZKYM         READE     MAZ01                                  60
     C                   Z-ADD     MAZRRN        AZRRN
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SETON                                        74  90
     C                   LEAVE
     C*
     C                   ENDIF                                                  XSTEMP<AZFEMP
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  VALIDATE AUTORIZATION TYPE
     C*
     C  N90SCAUTH        IFNE      'A'
     C     SCAUTH        ANDNE     'I'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCAUTH<>A/I
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AZDT          IFGT      SAVEDT
     C     AZDT          OREQ      SAVEDT
     C     AZTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AZDT>SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* KJDO   - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C*
     C                   SELECT
     C*
     C*---------------------------------------------------------------
     C*
     C     @@APPL        WHENEQ    'W'
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFND'
     C*
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '*'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *BLANKS       #@FND             3
     C*
     C     #@FND         IFEQ      *BLANKS
     C     #@FND         OREQ      *ZEROS
     C*
     C                   MOVE      *ON           *IN82
     C*
     C                   ELSE
     C*
     C                   MOVE      #@FND         SCFND
     C*
     C                   CLEAR                   FNNAME
     C     FNKYW         CHAIN     WFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C*
     C                   MOVE      *ON           *IN83
     C*
     C                   ENDIF                                                  #@FND=*BLANKS
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCTEMP'
     C*
     C     FNKYW         CHAIN     WFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        72  90
     C*
     C                   ELSE
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '*'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      WFNRRN        ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C*
     C                   OPEN      WMEMP
     C     ##RRN         CHAIN     WMEMP                              60
     C                   CLOSE     WMEMP
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *IN60         OREQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMP'
     C                   MOVE      *ON           *IN83
     C     HFLD          WHENEQ    'SCTEMP'
     C                   MOVE      *ON           *IN84
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMP'
     C                   Z-ADD     EMEMP#        SCFEMP
     C                   MOVE      *ON           *IN84
     C     HFLD          WHENEQ    'SCTEMP'
     C                   Z-ADD     EMEMP#        SCTEMP
     C                   MOVE      *ON           *IN85
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN=*BLANKS
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C*---------------------------------------------------------------
     C*
     C     @@APPL        WHENEQ    'M'
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'F0055R'                             99
     C                   PARM      '*'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ELSE
     C*
     C                   OPEN      FMSMP
     C     ##RRN         CHAIN     PSM0P                              60
     C                   CLOSE     FMSMP
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSNAM
     C*
     C                   MOVE      *ON           *IN82
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFND'
     C*
     C     SMKY          CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN71
     C*
     C                   ELSE
     C*
     C                   CALL      'F0060R'                             99
     C                   PARM      '*'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SMRRN         ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   MOVE      *ON           *IN82
     C*
     C                   ELSE
     C*
     C                   OPEN      FMFNP
     C     ##RRN         CHAIN     FMFNP                              60
     C                   CLOSE     FMFNP
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNAM
     C*
     C                   MOVE      *ON           *IN83
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCTEMP'
     C*
     C     FNKYM         CHAIN     MFN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        72  90
     C*
     C                   ELSE
     C*
     C                   CALL      'F0140R'                             99
     C                   PARM      '*'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      MFNRRN        ##RRN2            9 0
     C                   PARM      2             ##VIEW            1 0
     C*
     C                   OPEN      FMEMP
     C     ##RRN         CHAIN     FMEMP                              60
     C                   CLOSE     FMEMP
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *IN60         OREQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMP'
     C                   MOVE      *ON           *IN83
     C     HFLD          WHENEQ    'SCTEMP'
     C                   MOVE      *ON           *IN84
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEMP'
     C                   Z-ADD     EMEMP#        SCFEMP
     C                   MOVE      *ON           *IN84
     C     HFLD          WHENEQ    'SCTEMP'
     C                   Z-ADD     EMEMP#        SCTEMP
     C                   MOVE      *ON           *IN85
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN=*BLANKS
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C*---------------------------------------------------------------
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* LODPGT - Load the correct title for the program
     C*================================================================
     C*
     C     LODPGT        BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     'WCMS'        CAT       OPTNM1:1      OPTNM            40
     C     @@APPL        WHENEQ    'M'
     C     WCMC          CAT       OPTNM1:1      OPTNM
     C                   ENDSL
     C*
     C                   ENDSR                                                  LODPGT
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVE      'A'           AZSTAT
     C*
     C     *IN40         IFEQ      *OFF
     C     @@APPL        ANDEQ     'M'
     C                   Z-ADD     AZSUB#        SCSUB#
     C                   ENDIF                                                  *IN40=*OFF/@@APPL=M
     C*
     C     *IN40         IFEQ      *OFF
     C     @@APPL        ANDEQ     'W'
     C     @@APPL        OREQ      'M'
     C                   Z-ADD     AZFND         SCFND
     C                   ENDIF                                                  *IN40=*OFF/@@APPL=W
     C*
     C                   CLEAR                   SMNAME
     C                   CLEAR                   FNNAME
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     FNKYW         CHAIN     WFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C     @@APPL        WHENEQ    'M'
     C     SMKY          CHAIN     PSM01                              60
     C                   MOVEL(P)  SMNAME        SCSNAM
     C     FNKYM         CHAIN     MFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   ENDSL
     C*
     C                   Z-ADD     AZFEMP        SCFEMP
     C                   Z-ADD     AZTEMP        SCTEMP
     C                   MOVE      AZAUTH        SCAUTH
     C                   MOVE      AZSTAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCSUB#        AZSUB#
     C                   Z-ADD     SCFND         AZFND
     C                   Z-ADD     SCFEMP        AZFEMP
     C                   Z-ADD     SCTEMP        AZTEMP
     C                   MOVE      SCAUTH        AZAUTH
     C                   MOVE      SCSTAT        AZSTAT
     C*
     C                   MOVEL(P)  Q1USER        AZUSER
     C                   MOVEL(P)  Q1PGM         AZPGM
     C                   Z-ADD     DSTIME        AZTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AZDT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   CLOSE     WMFNL
     C     @@APPL        WHENEQ    'M'
     C                   CLOSE     FMSML
     C                   CLOSE     FMFNL
     C                   ENDSL
     C*
     C                   ENDSR                                                   PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CLEAR                   SCSUB#
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCSNAM
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   MOVE      *OFF          *IN30
     C                   OPEN      WMFNL
     C     @@APPL        WHENEQ    'M'
     C                   MOVE      *ON           *IN30
     C                   OPEN      FMSML
     C                   OPEN      FMFNL
     C                   ENDSL
     C*
     C     @@USR         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCUSNM
     C                   ELSE
     C     UPUSR         CAT(P)    UPUSNM:1      SCUSNM
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     ##CO#         AZCO#
     C                   MOVEL(P)  @@USR         AZUSR
     C                   MOVEL     @@APPL        AZAPPL
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL(P)  Q1USER        AZIUSR
     C                   MOVEL(P)  Q1PGM         AZIPGM
     C                   Z-ADD     DSTIME        AZITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AZIDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      SMAZP
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   MOVE      *ON           *IN83
     C     @@APPL        WHENEQ    'M'
     C                   MOVE      *ON           *IN82
     C                   ENDSL
     C*
     C                   ENDSR                                                  WRITER
