     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FM0010F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FMMOPP     IF   E             DISK
     F                                     RENAME(POP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FMMOPL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POP01:DBRCDF)
     FMMOPL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(POP01:DBRCDF2)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  SCDATA                 1     70
     D  MNTT                  12     51
     D  MNAM                  42     51
     D  APP                   53     55
     D  STAT                  58     58
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Menus')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('M0010R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(11)
     D*
     D* Program specific constants
     D*
     D DELETD          C                   CONST('*Deleted')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKMNAM
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKMNTT
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OPMNAM        KKMNAM
     C     *LIKE         DEFINE    OPMNTT        KKMNTT
     C     *LIKE         DEFINE    OPMNTT        U1DESC
     C     *LIKE         DEFINE    SSMNTT        U2DESC
     C     *LIKE         DEFINE    DBRRN         @@RRN
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSMNTT        IFNE      *BLANKS
     C     SSAPP         ORNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C                   SETON                                        11
E01  C                   ENDIF
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
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   MOVE      'A'           SSSTAT
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        KKMNAM
     C                   MOVEL     *LOVAL        KKMNTT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKMNAM
     C                   MOVEL     SCPST2        KKMNTT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCMNAM        KKMNAM
     C                   MOVEL     SCMNTT        KKMNTT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL(P)  OPMNAM        SCMNAM
     C                   MOVEL(P)  OPMNTT        SCMNTT
B01  C                   SELECT
S01  C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  OPMNAM        SCDATA
     C                   MOVEL     OPMNTT        MNTT
S02  C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  OPMNTT        SCDATA
     C                   MOVEL     OPMNAM        MNAM
E01  C                   ENDSL
     C                   MOVEL     OPAPP         APP
     C                   MOVEL     OPSTAT        STAT
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
B01  C                   SELECT
S01  C     *INKT         WHENEQ    *ON
     C                   MOVE      *BLANKS       ##CALL
B02  C     Q1PRMS        IFGT      0
     C                   MOVE      @@CALL        ##CALL
E02  C                   ENDIF
     C                   CALL      'M0012R'                             99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
E01  C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
B01  C                   SELECT
S01  C     SCSEL         WHENEQ    '7'
     C                   MOVE      *BLANKS       ##CALL
B02  C     Q1PRMS        IFGT      0
     C                   MOVE      @@CALL        ##CALL
E02  C                   ENDIF
     C                   CALL      'M0011R'                             99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      SCDBRR        ##RRN             9 0
S02  C     SCSEL         WHENEQ    '20'
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
E01  C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
B01  C     Q1PRMS        IFGE      3
     C                   MOVEL     *INKC         @@KC
     C                   MOVEL     *INKL         @@KL
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVEL     SCDBRR        @@RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
B01  C     SSMNTT        IFNE      *BLANKS
     C     LO:UP         XLATE     OPMNTT        U1DESC
     C     LO:UP         XLATE     SSMNTT        U2DESC
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1DESC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2DESC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF
E01  C                   ENDIF
     C*
B01  C     MEETSS        IFEQ      *ON
     C     SSAPP         ANDNE     *BLANKS
     C     SSAPP         ANDNE     OPAPP
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF
     C*
B01  C     MEETSS        IFEQ      *ON
     C     SSSTAT        ANDNE     *BLANKS
     C     SSSTAT        ANDNE     OPSTAT
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Name       Description                              App Sts
Opt  Description                              Name       App Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Menu options           YYN
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
219F19=All menu options     YYN
3
