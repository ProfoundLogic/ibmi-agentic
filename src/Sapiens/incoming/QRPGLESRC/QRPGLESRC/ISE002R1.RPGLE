     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  Created by C: 117  Project: 114IBW  04/05/01
     H*****************************************************************
     H*  REVISIONS:
     H*  06/20/01  C:117  NO SUPPORT SEQ#
     H*                   ADDED ANOTHER FIELD TO KEY
     H*****************************************************************
     FISE002F1  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FIME2P     IF   E             DISK
     F                                     RENAME(PE201:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEG CHG *** 06/20/01 ****************************************
     F****IME2L   IF  E           K        DISK
     F*** END CHG *** 06/20/01 ****************************************
     FIME2L2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PE201:DBRCDF)
     F*  File for Remark Code Description
     FSTCTL1    IF   E           K DISK
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
     D OPTNM           C                   CONST('Indexing Exit')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('ISE002R2')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*
     D*  Data Structures for VIEWS
     D*
     D                 DS
     D  VIEW1                  1     70
     D  V1FND                  3      5  0
     D  V1RMCD                11     15
     D  V1DESC                19     43
     D  V1AACT                49     49
     D  V1SACT                59     59
     D  V1STAT                70     70

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKFND
     C*** BEG ADD *** 06/20/01 ****************************************
     C                   KFLD                    KKRMCD
     C*** END ADD *** 06/20/01 ****************************************
     C*
     C*  Key list for Remark Code Description
     C*
     C     CTKY          KLIST
     C                   KFLD                    KKRMCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    E2FND         KKFND
     C     *LIKE         DEFINE    E2RMCD        KKRMCD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C     SSSTAT        IFNE      ' '
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
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKFND
     C*** BEG ADD *** 06/20/01 ****************************************
     C                   MOVE      *LOVAL        KKRMCD
     C*** END ADD *** 06/20/01 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKFND
     C*** BEG ADD *** 06/20/01 ****************************************
     C                   MOVEL     SCPSTA        KKRMCD
     C*** END ADD *** 06/20/01 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCFND         KKFND
     C*** BEG ADD *** 06/20/01 ****************************************
     C                   MOVEL     SCRMCD        KKRMCD
     C*** END ADD *** 06/20/01 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   VIEW1
     C*
     C*  Load data into hidden fields
     C*
     C                   Z-ADD     E2FND         SCFND
     C*** BEG ADD *** 06/20/01 ****************************************
     C                   MOVEL     E2RMCD        SCRMCD
     C*** END ADD *** 06/20/01 ****************************************
     C*
     C*  Load views
     C*
     C                   SELECT
     C*  VIEW 1
     C     *IN21         WHENEQ    '1'
     C                   Z-ADD     E2FND         V1FND
     C                   MOVEL     E2RMCD        V1RMCD
     C*
     C* Get description for REMARK CODE
     C                   MOVEL     E2RMCD        KKRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL     CTDESC        V1DESC
     C*
     C                   MOVEL     E2AACT        V1AACT
     C                   MOVEL     E2SACT        V1SACT
     C                   MOVEL     E2STAT        V1STAT
     C                   MOVEL     VIEW1         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - Subroutine for Option 20
     C*================================================================
     C     OPT20         BEGSR
     C*
     C*  Option 20=Change Status
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                    99
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
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     E2STAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Group ID  Code    Description                Action    Action      Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
