     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F2173R
     H* DESCRIPTION: WORK PATTERN TABLE FILE PROGRAM
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H* REVISIONS:
     H* 02/18/97 C:56  87MAZ
     H*                CREATED
     H*
     H* 05/23/97 C:56  PROJECT 87MAZ
     H*                1. ENABLE THIS PROGRAM TO BE CALLED AS
     H*                   SEARCH
     H*                2. INCOPORATE SCOPY CHANGES
     H**********************************************************
     FF2173F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFTWPP     IF   E             DISK
     F                                     RENAME(PWP01:PFRCDF)
     FFTWPL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PWP01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D*** BEGIN ADD *** 05/23/97 **************************************
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column Headings
     D*** END ADD *** 05/23/97 ****************************************
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D*** BEGIN DEL *** 05/23/97 **************************************
     D**********    'Work with Work Patt- C         OPTNM
     D**********    'ern - Selection'
     D*** END DEL *** 05/23/97 ****************************************
     D*
     D*** BEGIN ADD *** 05/23/97 **************************************
     D OPTNM           C                   CONST('Work Pattern')
     D*** END ADD *** 05/23/97 ****************************************
     D ACDPGM          C                   CONST('F2173R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D*** BEGIN ADD *** 05/23/97 **************************************
     D SF1PAG          C                   CONST(12)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     I*** END ADD *** 05/23/97 ****************************************
     I*
     I* Program specific constants
     I*

     C     *LIKE         DEFINE    WPKEY         KKKEY
     C     *LIKE         DEFINE    WPSTAT        KKSTAT
     C     *LIKE         DEFINE    WPSTAT        U1STAT
     C     *LIKE         DEFINE    WPSTAT        U2STAT
     C*** BEGIN ADD *** 05/23/97 **************************************
     C     *LIKE         DEFINE    WPKEY         @@KEY
     C     *LIKE         DEFINE    WPDESC        @@DESC
     C*** END ADD *** 05/23/97 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*** BEGIN ADD *** 05/23/97 **************************************
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@KEY
     C                   PARM                    @@DESC
     C*** END ADD *** 05/23/97 ****************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CO#             3 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
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
     C     SSSTAT        IFNE      'A'
     C     SSSTAT        ANDNE     'I'
     C     SSSTAT        ANDNE     ' '
     C                   MOVEA     '11'          *IN(89)
     C                   ENDIF
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
     C                   MOVE(P)   *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCKEY         KKKEY
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     WPKEY         SCKEY
     C                   MOVEL     WPDESC        SCDESC
     C                   MOVEL     WPSTAT        SCSTAT
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
B01  C                   SELECT
S01  C     SCSEL         WHENEQ    '20'
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
E01  C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD *** 05/23/97 **************************************
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
     C*** END ADD *** 05/23/97 ****************************************
     C                   MOVE      '1'           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN DEL *** 05/23/97 **************************************
     C**********           Z-ADD12        SF1PAG  50
     C**********           MOVELOPTNM     ##DATA    P
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C**********           MOVEL##DATA    SCPRDS
     C*** END DEL *** 05/23/97 ****************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C                   READP     DBRCDF                                 95
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
     C*** BEGIN ADD *** 05/23/97 **************************************
     C                   MOVEL     SCKEY         @@KEY
     C                   MOVEL     SCDESC        @@DESC
     C*** END ADD *** 05/23/97 ****************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSSTAT        ANDNE     WPSTAT
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF
     C                   ENDSR
** Column heading                ADDED  05/23/97
Opt Pattern   Description                               Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYN
12020=Change Status         YNN
203F3=Exit                  YYN
206F6=Add                   YNN
212F12=Cancel               YYY
213F13=User Parms           YYY
217F17=Subset               YYY
3
