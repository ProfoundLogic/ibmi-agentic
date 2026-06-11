     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7354R02 - EXTRACT SUBMISSION UNITS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7354R                                          *
     H*                                                               *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 06/06/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/14/16 C:136 Project: WC2070                               *
     H*                 Add 'Uncooperative Audit' description field   *
     H*                                                               *
     H*  11/10/03  C:15  Support: WC046/39822                         *
     H*                  Added option to delete policies.             *
     H*                                                               *
     H*  05/05/00  C:87  PROJECT: WC591                               *
     H*                  SCEMP# in display file was only 3 digits     *
     H*                  long.  Chg'd to 9 digits.                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FW7354F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMSRP     UF A E             DISK
     F                                     RENAME(PSR01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*** Begin add *** 11/10/03 **************************************
     FWMSQL     UF   E           K DISK
     FWDSEL     UF   E           K DISK
     FWDSLL     UF   E           K DISK
     F*** End add ***** 11/10/03 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Extracted Link and H-
     D                                     eader Info')
     D COMAPI          C                   CONST('WA001R')
     D*
     D*  Data structure for non-aswg policy condition fields
     D                 DS
     D  SCPCON                 1     12
     D  SCPC01                 1      1
     D  SCPC02                 2      2
     D  SCPC03                 3      3
     D  SCPC04                 4      4
     D  SCPC05                 5      5
     D  SCPC06                 6      6
     D  SCPC07                 7      7
     D  SCPC08                 8      8
     D  SCPC09                 9      9
     D  SCPC10                10     10
     D  SCPC11                11     11
     D  SCPC12                12     12
     D*
     D*  Data structure for aswg policy condition fields
     D                 DS
     D  SCPOLC                 1      7
     D  SCPL01                 1      1
     D  SCPL02                 2      2
     D  SCPL03                 3      3
     D  SCPL04                 4      4
     D  SCPL05                 5      5
     D  SCPL06                 6      6
     D  SCPL07                 7      7
     D*
     D*  Data policy ID fields
     D                 DS
     D  SCPID#                 1      6
     D  SCID01                 1      2  0
     D  SCID02                 3      4  0
     D  SCID03                 5      6  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*** Begin add *** 11/10/03 **************************************
     C*
     C     SEKY          KLIST
     C                   KFLD                    SRCO#
     C                   KFLD                    SRFND
     C                   KFLD                    SRRPMY
     C                   KFLD                    SREMP#
     C                   KFLD                    SRFYR
     C                   KFLD                    SRSTCD
     C*** End add ***** 11/10/03 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*** Begin add *** 11/10/03 **************************************
     C*
     C     SEKY          CHAIN     PSQ01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PSQ01
     C     SEKY          READE     PSQ01                                  99
     C                   ENDDO
     C*
     C     SEKY          CHAIN     PSL01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PSL01
     C     SEKY          READE     PSL01                                  99
     C                   ENDDO
     C*
     C     SEKY          CHAIN     PSE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PSE01
     C     SEKY          READE     PSE01                                  99
     C                   ENDDO
     C*** End add ***** 11/10/03 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*

     C*    SCR01
     C                   Z-ADD     SRCO#         KKCO#
     C                   Z-ADD     SRFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     SRFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   Z-ADD     SRCARR        SCCARR
     C*
     C                   Z-ADD     SREMP#        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     SREMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   Z-ADD     SRFYR         SCFYR
     C                   MOVE      SRCRT#        SCCRT#
     C                   Z-ADD     SRSTCD        SCSTCD
     C*
     C                   Z-ADD     SREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      SRRPT#        SCRPT#
     C                   MOVE      SRCORR        SCCORR
     C*
     C*    SCR02
     C                   Z-ADD     SRCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C*
     C                   MOVE      SRRSK#        SCRSK#
     C                   MOVE      SRPND#        SCPND#
     C                   MOVE      SRURRI        SCURRI
     C                   MOVE      SRRRPT        SCRRPT
     C*
     C*      Non-aswg fields
     C                   MOVE      SRPCON        SCPCON
     C                   Z-ADD     SRTERM        SCTERM
     C                   Z-ADD     SRPTYP        SCPTYP
     C*
     C*      ASWG fields
     C                   MOVE      SRPOLC        SCPOLC
      *** beg add ***** 10/14/16 ******************************
      /Free
       If scPl04 = 'U';
          scUnAudit = '(Uncooperative audit)';
       Else;
          scUnAudit = *blanks;
       Endif;
      /End-Free
      *** end add ***** 10/14/16 ******************************
     C                   MOVE      SRCTYP        SCCTYP
     C                   MOVE      SRPID#        SCPID#
     C                   Z-ADD     SRSTED        SCSTED
     C                   Z-ADD     SRFEIN        SCFEIN
     C*
     C*    SCR03 (aswg fields)
     C                   Z-ADD     SRDTYP        SCDTYP
     C                   Z-ADD     SRDPCT        SCDPCT
     C                   Z-ADD     SRDAMP        SCDAMP
     C                   Z-ADD     SRDAGG        SCDAGG
     C                   Z-ADD     SRPRP#        SCPRP#
     C                   MOVE      SRPCOR        SCPCOR
     C                   Z-ADD     SRPCAR        SCPCAR
     C                   MOVE      SRPPOL        SCPPOL
     C*
     C                   Z-ADD     SRPEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPEFD
     C*
     C                   Z-ADD     SRPEXP        SCPEXP
     C                   MOVE      SRPCER        SCPCER
     C                   MOVE      SRASWG        SCASWG
     C*
     C     SRASWG        IFNE      'A'
     C                   Z-ADD     2             #OFSCR
     C                   MOVE      *OFF          *IN91
     C                   ELSE
     C                   Z-ADD     3             #OFSCR
     C                   MOVE      *ON           *IN91
     C                   ENDIF                                                  SRASWG<>'A'
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     3             #OFSCR            1 0
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WMSRP
     C                   ENDSR
