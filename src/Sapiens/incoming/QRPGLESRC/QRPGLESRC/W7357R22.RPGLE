     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7357R22 - EXPOSURE RECORDS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7357R2                                         *
     H*                                                               *
     H*  CALL TO:  SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 02/28/05  PROGRAMMER: C:15  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7357F22  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDSEP2    UF A E             DISK
     F                                     RENAME(PSE01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  Unit totals file
     FWMSQP2L   UF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Exposure Records')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

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
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
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
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    SERPMY        KKRPMY
     C     *LIKE         DEFINE    SEFYR         KKFYR
     C     *LIKE         DEFINE    SESTCD        KKSTCD
     C     *LIKE         DEFINE    SECORR        KKCORR
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
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
     C*
     C                   DO
     C*
     C     SCMDDT        IFNE      0
     C                   Z-ADD     SCMDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMDDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN70
     C                   LEAVE
     C                   END
     C                   END
     C*
     C     SCRTDT        IFNE      0
     C                   Z-ADD     SCRTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRTDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN71
     C                   LEAVE
     C                   END
     C                   END
     C*
     C                   ENDDO
     C*
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
     C*===============================================================
     C* KPDO - SUBROUTINE FOR F15 = UNIT TOTALS
     C*===============================================================
     C     KPDO          BEGSR
     C*
     C*  Get the unit total record.
     C*
     C                   Z-ADD     SEEMP#        KKEMP#
     C                   Z-ADD     SERPMY        KKRPMY
     C                   Z-ADD     SEFYR         KKFYR
     C                   MOVE      SESTCD        KKSTCD
     C                   MOVE      SECORR        KKCORR
     C     KEY1          CHAIN(N)  PSQ01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Load the screen values and display the screen.
     C*
     C                   Z-ADD     SQEPAY        SCEPAY
     C                   Z-ADD     SQEOTH        SCEOTH
     C                   Z-ADD     SQSUBP        SCSUBP
     C                   Z-ADD     SQSTDP        SCSTDP
     C*
     C                   EXFMT     SCR02
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     KEY1          CHAIN     PSQ01                              60
     C                   Z-ADD     SCEPAY        SQEPAY
     C                   Z-ADD     SCEOTH        SQEOTH
     C                   Z-ADD     SCSUBP        SQSUBP
     C                   Z-ADD     SCSTDP        SQSTDP
     C                   UPDATE    PSQ01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     SECO#         KKCO#
     C                   Z-ADD     SEFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     SEFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     SERPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   Z-ADD     SEEMP#        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     SEEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   Z-ADD     SEFYR         SCFYR
     C                   MOVE      SECORR        SCCORR
     C*
     C                   MOVE      SECLAS        SCCLAS
     C                   Z-ADD     SEEXMD        SCEXMD
     C*
     C                   Z-ADD     SEMDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMDDT
     C*
     C                   Z-ADD     SERTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTDT
     C*
     C                   Z-ADD     SEEXPA        SCEXPA
     C                   Z-ADD     SEPRMA        SCPRMA
     C                   Z-ADD     SEMNRT        SCMNRT
     C                   MOVE      SESPLT        SCSPLT
     C                   MOVE      SETIER        SCTIER
     C                   MOVE      SEUPDT        SCUPDT
     C                   MOVE      SEEXPO        SCEXPO
     C                   MOVE      SESTCD        SCSTCD
     C*
     C     SEASWG        IFEQ      'Y'
     C                   MOVE      *ON           *IN91
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         SERPMY
     C*
     C                   Z-ADD     SCFND         SEFND
     C                   Z-ADD     SCEMP#        SEEMP#
     C                   Z-ADD     SCFYR         SEFYR
     C                   MOVE      SCCORR        SECORR
     C                   MOVE      SCSTCD        SESTCD
     C                   MOVE      SCUPDT        SEUPDT
     C                   MOVE      SCCLAS        SECLAS
     C                   Z-ADD     SCEXMD        SEEXMD
     C*
     C                   Z-ADD     SCMDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEMDDT
     C*
     C                   Z-ADD     SCRTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SERTDT
     C*
     C                   Z-ADD     SCEXPA        SEEXPA
     C                   Z-ADD     SCPRMA        SEPRMA
     C                   Z-ADD     SCMNRT        SEMNRT
     C                   MOVE      SCSPLT        SESPLT
     C                   MOVE      SCTIER        SETIER
     C                   MOVE      SCEXPO        SEEXPO
     C*
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
     C*
     C     *INKP         CASEQ     *ON           KPDO
     C                   ENDCS
     C*
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
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF
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
     C                   FEOD      WDSEP2
     C                   ENDSR
