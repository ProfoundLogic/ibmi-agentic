     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*                                                              *
     H* PROGRAM: F2400R                                              *
     H****************************************************************
     H* DESCRIPTION: WORK WITH STATE FS RULES                        *
     H* CALLED FROM: SY601R                                          *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 08/09/16 C:79  Support: 42770/114                            *
     H*                Allow 60 char search string on desc subset    *
     H*                                                              *
     H* 11/7/14  C:79  PROJECT FSMNT                                 *
     H*                Add F12                                       *
     H*                Populate state description                    *
     H*                                                              *
     H* 09/25/03 C:79  PROJECT TX020                                 *
     H*                REMOVE "PAGE" FIELD FROM VIEW1                *
     H*                                                              *
     H* 07/15/03 C:79  PROJECT TX020                                 *
     H*                ADD LOGIC TO CHANGE THE STATUS                *
     H*                                                              *
     H* 06/27/01 C:61  PROJECT FSMNT                                 *
     H*              - Cursor postion "Postion to" field.            *
     H*              - VIEW#1 - Swap 'Rule' and 'Effective Date'.    *
     H*                                                              *
     H*  01/23/01  C: 117  PROJECT: FSMNT #151                       *
     H*                    HAD TO MAKE A CHANGE TO PARMS FOR         *
     H*                    CALL TO F2400R01                          *
     H*                                                              *
     H* 12/14/00 C:079  PROJECT: FS218                               *
     H*                 Add STSTP and logic to chain to get the state*
     H*                                                              *
     H* 11/14/00 C:079  PROJECT: FSMNT                               *
     H*                 Temporarilay disable option 8=Copy to state  *
     H*                                                              *
     H* 04/18/00 C:079  PROJECT: TX010                               *
     H*                 ALLOW SYS TO BE P,H,D,X                      *
     H*                                                              *
     H* 09/11/98 C:110  PROJECT: FS138                               *
     H*                 SET F8 TO CALL F2400R02 RPG PROGRAM          *
     H*                                                              *
     H* 02/19/99 C:117  PROJECT: FS138                               *
     H*                 SUPPRESS LEADING ZEROS WHEN DISPLAYED        *
     H*                 CHANGE INITAL VIEW BY RULE # THEN EFF DT     *
     H*                                                              *
     H****************************************************************
     FF2400F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDFRP     IF   E             DISK
     F                                     RENAME(PFR01:PFRCDF)
     F*  Primary logical file definition (for displaying items)
     F*
     F*** BEG CHG *** 02/19/1999 **************************************
     F***********FDFRL   IF  E           K        DISK
     F***********                                   KINFDS DBFDS
     F*********** PFR01                             KRENAMEDBRCDF
     F*
     FFDFRL0    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFR01:DBRCDF)
     F*
     F*** END CHG *** 02/19/1999 **************************************
     F*
     F*  Primary logical file definition (for displaying VIEW 2)
     FFDFRL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PFR01:DBRCDF2)
     F*  Primary logical file definition (for displaying VIEW 3)
     FFDFRL2    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PFR01:DBRCDF3)
     F*  Primary logical file definition (for displaying VIEW 4)
     FFDFRL3    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PFR01:DBRCDF4)
     F*
     F*** BEG ADD *** 02/19/1999 **************************************
     F*
     F*  Primary logical file definition (for displaying VIEW 5)
     FFDFRL     IF   E           K DISK
     F                                     INFDS(DBFDS5)
     F                                     RENAME(PFR01:DBRCDF5)
     F*
     F*** END ADD *** 02/19/1999 **************************************
     F*** BEG ADD *** 12/14/00 ****************************************
     FSTSTP     IF   E             DISK
     F*** END ADD *** 12/14/00 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(5) CTDATA PERRCD(1)              Column headings
     D*** Beg CHG *** 11/07/14 ****************************************
     D*** OF              S              1    DIM(13) CTDATA PERRCD(1)          Options, Function
     D*** OFD             S             35    DIM(13) ALT(OF)
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*** End Chg *** 11/07/14 ****************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State Fee Schedule R-
     D                                     ules')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2400R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D*
     D*** BEG ADD *** 02/19/1999 **************************************
     D CHGNUM          C                   CONST('SA020R')
     D*** END ADD *** 02/19/1999 **************************************
     D*
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(5)
     D SF1PAG          C                   CONST(8)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D*
     D*  Date Data Structure used later for Century
     D                 DS
     D  XXEFDT                 1      8
     D  XXMTH                  1      2
     D  XXDAY                  3      4
     D  XXYEAR                 5      8
     D*
     D*** BEG CHG *** 02/19/1999 **************************************
     D*********** DS
     D***********                             1  37 VIEW1
     D***********                             1  10 X1EFDT
     D***********                            12  14 V1PAGE
     D***********                            16  250V1RULE
     D***********                            27  37 V1PCAT
      ***** BEG DEL ******* 06/27/01 **********************************
      ***********
     D*********** DS
     D***********                             1  37 VIEW1
     D***********                             1  10 X1EFDT
     D***********                            12  21 V1RULE
     D***********                            23  25 V1PTY
     D***********                            27  37 V1PCAT
     D***********
     D****END*CHG *** 02/19/1999 **************************************
      ***** END DEL ******* 06/27/01 **********************************
      ***** BEG ADD ******* 06/27/01 **********************************
      *
      *  VIEW 1 Data Structure
      *
     D                 DS
     D*** BEG CHG *** 09/25/03 ****************************************
     D*************************************** 1  37 VIEW1
     D  VIEW1                  1     33
     D*** END CHG *** 09/25/03 ****************************************
     D  V1RULE                 1      9
     D  X1EFDT                12     21
     D  V1PTY                 23     25
     D*** BEG CHG *** 09/25/03 ****************************************
     D************************************** 27  37 V1PCAT
     D  V1PCAT                28     33
     D*** END CHG *** 09/25/03 ****************************************
     D*
      ***** END ADD ******* 06/27/01 **********************************
     D*
     D*  VIEW 2 Data Structure
     D                 DS
     D  VIEW2                  1     37
     D  V2PCAT                 1     10
     D  X2EFDT                12     22
     D  V2PAGE                24     27
     D  V2RULE                29     37
     D*
     D*  VIEW 3 Data Structure
     D                 DS
     D  VIEW3                  1     37
     D  V3RULE                 1      9
     D  X3EFDT                11     21
     D  V3PAGE                23     25
     D  V3PCAT                27     37
     D*
     D*  VIEW 4 Data Structure
     D                 DS
     D*** BEG CHG *** 09/25/03 ****************************************
     D*************************************** 1  37 VIEW4
     D  VIEW4                  1     33
     D*** END CHG *** 09/25/03 ****************************************
     D  X4EFDT                 1     10
     D  V4PTY                 12     14
     D  V4RULE                16     25
     D*** BEG CHG *** 09/25/03 ****************************************
     D************************************** 27  37 V4PCAT
     D  V4PCAT                28     33
     D*** END CHG *** 09/25/03 ****************************************
     D*
     D*** BEG ADD *** 02/19/1999 **************************************
     D*
     D*  VIEW 5 Data Structure
     D                 DS
     D  VIEW5                  1     37
     D  X5EFDT                 1     10
     D  V5PAGE                12     14
     D  V5RULE                16     25
     D  V5PCAT                27     37
     D*
     D*** END ADD *** 02/19/1999 **************************************
     D*
     D*  VIEW INFOR  for second string -- setup for later additions
     D                 DS
     D  INFO1                  1     32
     D  S1PTY                  1      3
     D  S1DESC                 5     32
     D*
     D*  VIEW INFO2
     D*** BEG ADD *** 09/25/03 ****************************************
     D                 DS
     D  INFO2                  1     38
     D  S2DESC                 1     38

     I*** END ADD *** 09/25/03 ****************************************
     I*** BEG DEL *** 09/25/03 ****************************************
     I***********
     I*********** DS
     I***********                             1  32 INFO2
     I***********                             1   3 S2PAGE
     I***********                             5  32 S2DESC
     I*** END ADD *** 09/25/03 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN3            9 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCST          ##ST              2
     C                   PARM                    ##CO#             3 0
     C*** BEG ADD *** 01/23/01 ****************************************
     C                   PARM      @@RRN3        ##RRN3            9 0
     C*** END ADD *** 01/23/01 ****************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   PAGE UP KEY
     C*
     C     KEY           KLIST
     C                   KFLD                    KKST
     C*
     C*   VIEW 1
     C*
     C     KEY1          KLIST
     C*** BEG CHG *** 02/19/1999 **************************************
     C                   KFLD                    KKST
     C***********          KFLD           KKPAGE
     C                   KFLD                    KKRULE
     C                   KFLD                    KKEFDT
     C*** END CHG *** 02/19/1999 **************************************
     C*
     C*   VIEW 2
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKPCAT
     C                   KFLD                    KKRULE
     C*
     C*   VIEW 3
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C*
     C*   VIEW 4
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKPTY
     C                   KFLD                    KKRULE
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C*
     C*   VIEW 5
     C*
     C     KEY5          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKPAGE
     C                   KFLD                    KKRULE
     C*
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FRST          KKST
     C     *LIKE         DEFINE    FREFDT        KKEFDT
     C     *LIKE         DEFINE    FRPAGE        KKPAGE
     C     *LIKE         DEFINE    FRPCAT        KKPCAT
     C     *LIKE         DEFINE    FRRULE        KKRULE
     C     *LIKE         DEFINE    FRPTY         KKPTY
     C     *LIKE         DEFINE    FRDESC        U1PDSC
     C     *LIKE         DEFINE    FRDESC        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSPTY         IFNE      *ZEROS
     C     SSSTDS        ORNE      *BLANKS
     C     SSMESG        ORNE      *BLANKS
     C     SSEOB         ORNE      *BLANKS
     C     SSPCAT        ORNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSSYS         ORNE      *BLANKS
     C*** BEG ADD *** 07/15/03 ****************************************
     C     SSSTAT        ORNE      *BLANKS
     C*** END ADD *** 07/15/03 ****************************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C                   DO
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   LEAVE
     C                   ENDIF
     C*
     C* VERIFY EFFECTIVE DATE IS A VALID CODE
     C*
     C  N90SSEFDT        IFNE      *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   ENDIF                                                  END SRERR
     C                   LEAVE
     C                   ENDIF                                                  END NE BLANK
     C*
     C* VERIFY SYSTEM TYPE IS VALID
     C*
     C  N90SSSYS         IFNE      'P'
     C     SSSYS         ANDNE     'H'
     C*** BEG ADD *** 04/19/00 ****************************************
     C     SSSYS         ANDNE     'D'
     C     SSSYS         ANDNE     'X'
     C*** END ADD *** 04/19/00 ****************************************
     C     SSSYS         ANDNE     *BLANKS
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF                                                  END SSSYS
     C*** BEG ADD *** 07/15/03 ****************************************
     C*
     C* VERIFY STATUS
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF                                                  END SSSTAT
     C*** END ADD *** 07/15/03 ****************************************
     C*
     C                   ENDDO
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*** BEG ADD *** 07/15/03 ****************************************
     C                   MOVE      'A'           SSSTAT
     C*** END ADD *** 07/15/03 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE(P)   *LOVAL        KKEFDT
     C                   MOVE(P)   *LOVAL        KKPAGE
     C                   MOVE(P)   *LOVAL        KKPCAT
     C                   MOVE(P)   *LOVAL        KKRULE
     C                   MOVE(P)   *LOVAL        KKPTY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      *LOVAL        KKRULE
     C     *IN21         IFEQ      *ON
     C     *IN24         OREQ      *ON
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C     *IN25         OREQ      *ON
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C   21              MOVEL(P)  SCPSTA        XXDAT8            8 0
     C   24              MOVEL(P)  SCPST4        XXDAT8
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25              MOVEL(P)  SCPST5        XXDAT8
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   Z-ADD     XXDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        KKEFDT
     C*
     C*** BEG CHG *** 02/19/1999 **************************************
     C**********21                MOVELSCPSTA    KKPAGE    P
     C   21              MOVEL(P)  SCPST1        KKRULE
     C*** END CHG *** 02/19/1999 **************************************
     C*
     C   24              MOVEL(P)  SCPSTB        KKPTY
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25              MOVEL(P)  SCPSTC        KKPAGE
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   ENDIF                                                  *IN21 = On
     C   22              MOVEL(P)  SCPST2        KKPCAT
     C   23              MOVEL(P)  SCPST3        KKRULE
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   MOVEL(P)  SCPAGE        KKPAGE
     C                   MOVEL(P)  SCPCAT        KKPCAT
     C                   MOVEL(P)  SCRULE        KKRULE
     C                   Z-ADD     SCPTY         KKPTY
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   CLEAR                   SCDATA
     C*** END ADD *** 09/25/03 ****************************************
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C                   CLEAR                   VIEW3
     C                   CLEAR                   VIEW4
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C                   CLEAR                   VIEW5
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   CLEAR                   INFO1
     C                   CLEAR                   INFO2
     C*
     C                   Z-ADD     FREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C                   MOVEL(P)  XXEFDT        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXEFDT
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   Z-ADD     FREFDT        SCEFDT
     C*** Beg Del *** 11/07/14 **************************************
     C***************    MOVE      *BLANKS       SCSTDS
     C*** End Del *** 11/07/14 **************************************
     C*
     C                   MOVE      FRPAGE        SCPAGE
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C                   MOVEL(P)  SCPAGE        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C*** BEG DEL *** 09/25/03 ****************************************
     C******************** MOVEL##NUM     S2PAGE
     C*** END DEL *** 09/25/03 ****************************************
     C                   MOVEL     ##NUM         V2PAGE
     C                   MOVEL     ##NUM         V3PAGE
     C                   MOVEL     ##NUM         V5PAGE
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   MOVE      FRPCAT        SCPCAT
     C*
     C*** BEG CHG *** 02/19/1999 **************************************
     C***********          MOVE FRRULE    SCRULE
     C                   MOVE      FRRULE        XXRULE            9 0
     C*** END CHG *** 02/19/1999 **************************************
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C                   MOVEL(P)  XXRULE        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         V1RULE
     C                   MOVEL     ##NUM         V2RULE
     C                   MOVEL     ##NUM         V3RULE
     C                   MOVEL     ##NUM         V4RULE
     C                   MOVEL     ##NUM         V5RULE
     C                   MOVEL     XXRULE        SCRULE
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   MOVE      FRPTY         SCPTY
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C                   MOVEL(P)  SCPTY         ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         S1PTY
     C                   MOVEL     ##NUM         V1PTY
     C                   MOVEL     ##NUM         V4PTY
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C* LOAD INFORMATION FOR PRIORITY AND DESCRIPTION
     C*
     C* INFO2
     C     *IN24         IFEQ      '1'
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C     *IN21         OREQ      '1'
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE SCPAGE    S2PAGE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C                   MOVEL     FRDESC        S2DESC
     C                   MOVE      INFO2         SCDATA
     C                   ELSE
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE FRPTY     S1PTY
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C                   MOVEL     FRDESC        S1DESC
     C                   MOVE      INFO1         SCDATA
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* VIEW 1
     C     *IN21         WHENEQ    '1'
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE FRPAGE    V1PAGE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C*** BEG CHG *** 09/25/03 ****************************************
     C******************** MOVE FRPCAT    V1PCAT    P
     C                   MOVEL(P)  FRPCAT        V1PCAT
     C*** END CHG *** 09/25/03 ****************************************
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVELSCRULE    V1RULE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C     XXMTH         CAT       '/':0         X1EFDT
     C                   CAT       XXDAY:0       X1EFDT
     C                   CAT       '/':0         X1EFDT
     C                   CAT       XXYEAR:0      X1EFDT
     C                   MOVEL     VIEW1         SCDATA
     C*
     C* VIEW 2
     C     *IN22         WHENEQ    '1'
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE FRPAGE    V2PAGE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C                   MOVE      FRPCAT        V2PCAT
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          Z-ADDFRRULE    V2RULE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C     XXMTH         CAT       '/':0         X2EFDT
     C                   CAT       XXDAY:0       X2EFDT
     C                   CAT       '/':0         X2EFDT
     C                   CAT       XXYEAR:0      X2EFDT
     C                   MOVEL     VIEW2         SCDATA
     C*
     C* VIEW 3
     C     *IN23         WHENEQ    '1'
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE FRPAGE    V3PAGE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C                   MOVE      FRPCAT        V3PCAT
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          Z-ADDFRRULE    V3RULE
     C*** END DEL *** 02/19/1999 **************************************
     C*
     C     XXMTH         CAT       '/':0         X3EFDT
     C                   CAT       XXDAY:0       X3EFDT
     C                   CAT       '/':0         X3EFDT
     C                   CAT       XXYEAR:0      X3EFDT
     C                   MOVEL     VIEW3         SCDATA
     C*
     C*
     C* VIEW 4
     C     *IN24         WHENEQ    '1'
     C*
     C*** BEG DEL *** 02/19/1999 **************************************
     C***********          MOVE FRPTY     V4PTY
     C*** END DEL *** 02/19/1999 **************************************
     C*** BEG CHG *** 09/25/03 ****************************************
     C******************** MOVE FRPCAT    V4PCAT
     C                   MOVEL     FRPCAT        V4PCAT
     C*** END CHG *** 09/25/03 ****************************************
     C*                    Z-ADDFRRULE    V4RULE
     C*
     C     XXMTH         CAT       '/':0         X4EFDT
     C                   CAT       XXDAY:0       X4EFDT
     C                   CAT       '/':0         X4EFDT
     C                   CAT       XXYEAR:0      X4EFDT
     C                   MOVEL     VIEW4         SCDATA
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C* VIEW 5
     C     *IN25         WHENEQ    '1'
     C*                    MOVE FRPAGE    V5PAGE
     C                   MOVE      FRPCAT        V5PCAT
     C*                    Z-ADDFRRULE    V5RULE
     C*
     C     XXMTH         CAT       '/':0         X5EFDT
     C                   CAT       XXDAY:0       X5EFDT
     C                   CAT       '/':0         X5EFDT
     C                   CAT       XXYEAR:0      X5EFDT
     C                   MOVEL     VIEW5         SCDATA
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   ENDSL
     C*** BEG ADD *** 07/15/03 ****************************************
     C                   MOVEL     FRSTAT        SCSTAT
     C*** END ADD *** 07/15/03 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*******************
     C* INK=SEARCH      *
     C*******************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***********************
     C*   PROCEDURE CATEGORY*
     C***********************
     C*
     C     HFLD          IFEQ      'SSPCAT'
     C*
     C                   MOVE      'PCAT'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C                   SETON                                        90
     C*
     C                   MOVEL(P)  QQKEY         SSPCAT
     C*
     C                   ENDIF                                                  END CATEGORY
     C***********************
     C*   SYSTEM TYPE       *
     C***********************
     C*
     C     HFLD          IFEQ      'SSSYS'
     C*
     C                   MOVE      'SYS'         QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C                   SETON                                        90
     C*
     C                   MOVEL(P)  QQKEY         SSSYS
     C*
     C                   ENDIF                                                  END SYSTEM
     C***********************************
     C*  EOB CODE SEARCH                *
     C***********************************
     C*
     C     HFLD          IFEQ      'SSEOB'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'F2131R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQST              2
     C*
     C                   SETON                                        90
     C*
     C                   MOVEL(P)  QQKEY         SSEOB
     C*
     C                   ENDIF                                                  END SCEOB
     C***********************************
     C*  ERROR MESSAGES                 *
     C***********************************
     C*
     C     HFLD          IFEQ      'SSMESG'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'F2385R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQTYPE            1
     C                   PARM      SCST          QQST              2
     C                   PARM                    QQMSG#            7
     C*
     C                   SETON                                        90
     C*
     C                   MOVEL(P)  QQMSG#        SSMESG
     C*
     C                   ENDIF                                                  END SCMSG
     C                   ENDIF                                                  END *INKJ
     C*
     C*** BEG ADD *** 09/11/98 ****************************************
     C     *INKH         IFEQ      *ON
     C*
     C                   CALL      'F2400R02'
     C                   PARM      'A'           ##MODE            1
     C     *INKC         PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM                    ##CO#             3 0
     C*
     C                   SETON                                        90
     C                   ENDIF
     C*** END ADD *** 09/11/98 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         IFEQ      '8'
     C*
     C*** BEG DEL *** 11/14/00 ****************************************
     C******************** CALL 'F2400R02'
     C******************** PARM           @@KC    1
     C******************** PARM           @@KL    1
     C******************** PARM           @@MODE  1
     C******************** PARM           @@RRN   90
     C******************** PARM 'C'       XXCPY   1
     C******************** SETON                         90
     C*** END DEL *** 11/14/00 ****************************************
     C                   ENDIF                                                   END SEL =8
     C*** BEG ADD *** 07/15/03 ****************************************
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*** END ADD *** 07/15/03 ****************************************
     C*
     C                   ENDSR
     C*** BEG ADD *** 07/15/03 ****************************************
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
     C*** END ADD *** 07/15/03 ****************************************
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      '1'           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@RRN3        CHAIN     PST01                              99
     C*** Beg Add *** 11/07/14 **************************************
     C                   if        *in99 = *off
     C*** End Add *** 11/07/14 **************************************
     C*
     C                   MOVEL     STKEY         SCST
     C                   MOVEL     STKEY         KKST
     C*** Beg Add *** 11/07/14 **************************************
     C                   eval      scStDs = STName
     C*
     C                   EndIf
     C*** End Add *** 11/07/14 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
     C   24KEY           READE     DBRCDF4                                95
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25KEY           READE     DBRCDF5                                95
     C*** END ADD *** 02/19/1999 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
     C   24KEY           READPE    DBRCDF4                                95
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25KEY           READPE    DBRCDF5                                95
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25KEY5          SETGT     DBRCDF5                            96
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C*** BEG ADD *** 02/19/1999 **************************************
     C*          GO        IFEQ *ON
     C*                    EXSR FDRULE
     C*                    ELSE
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C*** BEG ADD *** 02/19/1999 **************************************
     C   25KEY5          SETLL     DBRCDF5                            96
     C*                    ENDIF
     C*** END ADD *** 02/19/1999 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVE      DL            SCDATA
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
     C     SSSTDS        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  FRDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSSTDS        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C*** Beg Chg *** 08/09/16 **********************************
     C*************      PARM      40            STRLEN            3 0
     C                   PARM      60            STRLEN            3 0
     C*** End Chg *** 08/09/16 **********************************
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  NRES = 0
     C                   ENDIF                                                  PDSC <> Blanks
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     XXDATE        IFNE      0
     C     XXDATE        ANDNE     FREFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSPCAT        IFNE      *BLANK
     C     LO:HI         XLATE(P)  FRPCAT        U1PDSC
     C     LO:HI         XLATE(P)  SSPCAT        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  NRES = 0
     C                   ENDIF
     C*
     C     SSEOB         IFNE      *BLANK
     C     SSEOB         ANDNE     FREOB
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSMESG        IFNE      *BLANK
     C     SSMESG        ANDNE     FRMSG
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSPTY         IFNE      0
     C     SSPTY         ANDNE     FRPTY
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSYS         IFNE      *BLANK
     C     SSSYS         ANDNE     FRSYS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEG ADD *** 07/15/03 ****************************************
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     FRSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END ADD *** 07/15/03 ****************************************
     C                   ENDSR
     C*
** Column headings
Opt      Rule   Eff Date  Pty  Categ  Description                          Sts
Opt Category   Eff Date     Page     Rule   Pty Description                Sts
Opt      Rule Eff Date    Page Category     Pty Description                Sts
Opt Eff Date   Pty      Rule   Categ  Description                          Sts
Opt Eff Date   Page     Rule   Category     Pty Description                Sts
** Description--------------MISN
1022=Change                 YYN
1033=Copy                   YYN
1044=Delete                 YNN
1055=Display                YYN
1088=Copy to state          YYN
12020=Change Status         YYN
203F3=Exit                  YYN
205F5=Refresh               YYN
206F6=Add                   YNN
208F8=Rollover              YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
