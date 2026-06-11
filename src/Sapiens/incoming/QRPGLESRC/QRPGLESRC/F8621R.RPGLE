     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *
      *****************************************************************
      *  F8621R - WORK WITH COVENTRY VALIDATION FILES                 *
      *                                                               *
      *  CREATED  :  5/19/2008   PROGRAMMER: 47    PROJECT: 129MPN    *
      *                                                               *
      *  CALLED BY:  MENU                                             *
      *  CALLS TO :  none                                             *
      *                                                               *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
     FF8621F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFIY1P     IF   E             DISK
     F                                     RENAME(PY101:PFRCDF)
     F*
     F*  Using pos 1-3 of 240a field. Select on rcd type of '101'.
     FFIY1L1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PY101:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
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
     D OPTNM           C                   CONST('Coventry Validation -
     D                                     Files')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F8621R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D* Record 101
     D V101            DS                  INZ
     D  V0101                  1      3  0
     D  V0102                  4      5
     D  V0103                  6     25
     D  V0104                 26     45
     D  V0105                 46     65
     D  V0106                 66     85
     D  V0106A                66     74
     D  V0106B                75     77
     D  V0107                 86    105
     D  CSUB                  86     90  0
     D  CFND                  91     93  0
     D  CFYR                  94     96  0
     D  CCASE                 97    103  0
     D  V0108                106    108  0
     D  V0109                109    113
     D  V0110                114    183
     D  V0111                184    191
     D  V0112                192    195  0
     D  V0113                196    196
     D  V0198                197    216  0
     D  V0199                217    236

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KL              1
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
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKRTYP
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRSEQ
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    Y1RTYP        KKRTYP
     C     *LIKE         DEFINE    Y1EOB#        KKEOB#
     C     *LIKE         DEFINE    Y1RSEQ        KKRSEQ
     C     *LIKE         DEFINE    ##RRN         @@RRN
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKRTYP
     C                   MOVE      *ZERO         KKEOB#
     C                   MOVE      *ZERO         KKRSEQ
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  '101'         KKRTYP
     C                   MOVEL(P)  SCPST1        KKEOB#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCRTYP        KKRTYP
     C                   MOVE      SCEOB#        KKEOB#
     C                   MOVE      SC0112        KKRSEQ
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  Y1DATA        V101
     C                   MOVEL(P)  Y1RTYP        SCRTYP
     C                   MOVE(P)   V0106A        SCEOB#
     C                   MOVE(P)   V0106B        SCRCN
     C*
     C***                  MOVE V0105     SRDTI
     C***                  CALL 'SRCVTDAT'DTMDY2
     C***                  MOVELSRDTO     SCRCDT
     C*
     C                   MOVEL(P)  V0110         SC0110
     C                   MOVE      V0112         SC0112
     C                   MOVEL     V0113         SC0113
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - Release from reject queque
     C*================================================================
     C     OPT20         BEGSR
     C*
     C* ONLY GO TO THE MAINT SCREEN IF THE BILLS WERE NOT ACCEPTED.
     C*
     C     V0113         IFNE      'Y'
     C*
     C* THIS WILL CALL THE MAINT PGM TO UPDATE FAEAP, FIY1P.
     C* UPON RETURN, THE RECORD WILL BE REMOVED FROM THE SUBFILE.
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   MOVE      *ON           AUTORF
     C*
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT20                          RELEASE
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           @@KL
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
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C* ONLY DISPLAY RECORDS THAT HAVE NOT BEEN PROCESSED.
     C*
     C     Y1PRFL        IFNE      *BLANK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C* ONLY DISPLAY '101' RECORDS
     C*
     C                   MOVEL(P)  Y1DATA        V101
     C     V0101         IFNE      101
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C* ONLY DISPLAY RECORDS THAT HAVE ERRORS
     C*
     C     V0113         IFEQ      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
      * ===========================================================
      * 1         2         3         4         5         6
234567*9012345678901234567890123456789012345678901234567890123456
** Column headings
Opt     EOB#    Recon#  Seq#  Accepted?  Error messages
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
12020=Release from reject Q YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
