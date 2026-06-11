     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               * *
     H*  09/14/00 C:62 PROJECT:URMNT                                  * *
     H*                COMPILE ONLY, SEE U2300F.                      * *
     H*                                                               *
     H*****************************************************************
     FU2300F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FUMLSP     IF   E             DISK
     F                                     RENAME(PLS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FUMLSL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLS01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('LOS Data')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('U2300R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  DATE CONVERSION FROM MDYY TO CYMD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKRGN
     C                   KFLD                    KKLGRP
     C                   KFLD                    KKCAT
     C                   KFLD                    KKAGE
     C                   KFLD                    KKEFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LSRGN         KKRGN
     C     *LIKE         DEFINE    LSLGRP        KKLGRP
     C     *LIKE         DEFINE    LSLGRP        S1LGRP
     C     *LIKE         DEFINE    LSCAT         KKCAT
     C     *LIKE         DEFINE    LSAGE         KKAGE
     C     *LIKE         DEFINE    LSEFDT        KKEFDT
     C     *LIKE         DEFINE    LSEFDT        SSEFD1
     C     *LIKE         DEFINE    SCDATE        XXDATE
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSRGN         IFNE      *ZEROS
B01  C     SSLGRP        ORNE      *ZEROS
B01  C     SSCAT         ORNE      *BLANKS
B01  C     SSAGE         ORNE      *ZEROS
B01  C     SSEFDT        ORNE      *ZEROS
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        8890
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSEFD1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKRGN
     C                   MOVE      *LOVAL        KKLGRP
     C                   MOVE      *LOVAL        KKCAT
     C                   MOVE      *LOVAL        KKAGE
     C                   MOVE      *LOVAL        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKRGN
     C                   MOVEL(P)  SCPSTG        KKLGRP
     C                   MOVE      SCPSTC        KKCAT
     C                   MOVE      SCPSTA        KKAGE
     C*
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCRGN         KKRGN
     C                   MOVEL(P)  SCLGRP        KKLGRP
     C                   MOVE      SCCAT         KKCAT
     C                   MOVE      SCAGE         KKAGE
     C                   MOVE      SCEFDT        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      LSRGN         SCRGN
     C                   MOVEL(P)  LSLGRP        SCLGRP
     C                   MOVEL(P)  LSLGRP        SCLGP2
     C                   MOVE      LSCAT         SCCAT
     C                   MOVE      LSAGE         SCAGE
     C                   MOVE      LSEFDT        SCEFDT
     C*
     C                   Z-ADD     LSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVEL     LSSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
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
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                               9595
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                               9595
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C     KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C     KEY1          SETLL     DBRCDF                             96
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
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSRGN         IFNE      *ZEROS
     C     SSRGN         ANDNE     LSRGN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSLGRP        IFNE      *ZEROS
     C                   MOVEL     SSLGRP        S1LGRP
     C     S1LGRP        IFNE      LSLGRP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSCAT         IFNE      *BLANKS
     C                   MOVE      SSCAT         SSCAT1            1 0
     C     SSCAT1        IFNE      LSCAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSAGE         IFNE      *ZEROS
     C     SSAGE         ANDNE     LSAGE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEFDT        IFNE      *ZEROS
     C*
     C     SSEFD1        IFNE      LSEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     LSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Region   Group    Category     Age         Eff. date        Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
