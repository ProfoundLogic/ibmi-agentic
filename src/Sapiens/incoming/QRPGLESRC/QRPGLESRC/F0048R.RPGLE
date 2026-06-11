     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE F0048R - WORK WITH PROVIDER DRG RATES
     H*****************************************************************
     H** CREATED ON 01/08/02 BY C:62 FOR PROJECT WV004
     H*****************************************************************
     H*  REVISIONS:
     H*                                                                 *
     H*  08/06/08  C:33  Project: SY156  Priss #26                      *
     H*                  Update subset FEIN processing                  *
     H*****************************************************************
     FF0048F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMP2P     IF   E             DISK
     F                                     RENAME(PP201:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMP2L2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PP201:DBRCDF)
     FFMPDL     IF   E           K DISK
     FSTABL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             75    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  ##CMD1                 1     56
     D  ##ST1                 38     39
     D                 DS
     D  ##CMD2                 1     56
     D  ##ST2                 40     41
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider DRG Rates')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0048R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D ZEROS           C                   CONST('000000000')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@SV01           11
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  CONVERTS DATE FROM MDYY TO CYMD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKSV01
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    P2NAME        KKNAME
     C     *LIKE         DEFINE    P2EFDT        KKEFDT
     C     *LIKE         DEFINE    P2EFDT        S1EFDT
     C     *LIKE         DEFINE    P2SV01        KKSV01
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDFEIN        S1FEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    STKEY         KKST
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSFEIN        IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSEFDT        ORNE      *ZEROS
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* DOVSVP - Delete Override FMSVP files
     C*================================================================
     C     DOVSVP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH           15 5
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C* FEIN/MOD MUST BE VALID IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SSFEIN        ANDNE     *BLANKS
     C     SSFEIN        ANDNE     ZEROS
     C     *IN90         OREQ      *OFF
     C     SSMOD         ANDNE     *ZEROS
     C                   MOVE      SSFEIN        KKFEIN
     C                   Z-ADD     SSMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ELSE
     C                   Z-ADD     PDFEIN        S1FEIN
     C                   ENDIF
     C                   ELSE
     C     SSFEIN        IFEQ      ZEROS
     C                   MOVE      SSFEIN        S1FEIN
     C                   MOVEL     *BLANKS       SCPNAM
     C                   ENDIF
     C     SSFEIN        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCPNAM
     C                   ENDIF
     C                   ENDIF
     C*
     C* EFFECTIVE DATE MUST BE VALID IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   ELSE
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C                   ENDIF
     C                   ENDIF
     C*
     C* STATUS MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSTAT        ANDNE     ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C***  Begin Add  ***  08/06/08  **************************************
     C**********           MOVE *ZEROS    SSFEIN
     C                   MOVE      *BLANKS       SSFEIN
     C***  End   Add  ***  08/06/08  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKNAME
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKSV01
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL     SCPST1        KKNAME
     C   21SCPSTD        IFNE      *ZEROS
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL(P)  SCNAME        KKNAME
     C   21              Z-ADD     SCEFDT        KKEFDT
     C   21              MOVEL(P)  SCSV01        KKSV01
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SSFEIN'
     C     HFLD          OREQ      'SSMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      *BLANKS       ##1N4             1            type 1 & 4 only
     C                   PARM      *BLANKS       ##1PRV            1            Y=subset on fein/mod
     C                   PARM      *ZEROS        ##PRIV            1 0          VIEW
     C                   PARM      @@ST          ##PST             2            ADJUDICATION ST
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      ##FEIN        SSFEIN
     C                   MOVE      ##MOD         SSMOD
     C                   MOVE      ##FEIN        KKFEIN
     C                   MOVE      ##MOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCPNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPNAM
     C                   ENDIF
     C                   SETON                                        32
     C                   ENDIF                                                  ##RTCD = A
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSFEIN
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL     STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     P2NAME        SCNAME
     C                   MOVEL     P2NAME        SCNAM1
     C                   Z-ADD     P2EFDT        SCEFDT
     C                   Z-ADD     P2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD8
     C                   MOVEL     P2SV01        SCSV01
     C                   Z-ADD     P2FEIN        SCFEIN
     C                   Z-ADD     P2MOD         SCMOD
     C                   MOVEL     P2STAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OVRSVP - Override FMSVP files
     C*================================================================
     C     OVRSVP        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        ##CMD1
     C                   MOVEL(P)  @@ST          ##ST1
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD1
     C                   PARM                    LENGTH
     C*
     C                   MOVEL(P)  QCA(2)        ##CMD2
     C                   MOVEL(P)  @@ST          ##ST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD2
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   EXSR      DOVSVP
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C                   EXSR      OVRSVP
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCSV01        @@SV01
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSFEIN        IFNE      *BLANKS
     C***  Begin Add  ***  08/06/08  **************************************
     C     SSFEIN        ANDNE     *ZEROS
     C***  End   Add  ***  08/06/08  **************************************
     C     S1FEIN        ANDNE     P2FEIN
     C                   MOVEL     *OFF          MEETSS
     C     SSMOD         IFNE      P2MOD
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     P2EFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     P2STAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Name                                Eff. Date   DRG Code    Fein    Mod Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
**  (QCMDEX)
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMSVP)
DLTOVR FILE(FMSVL1)
