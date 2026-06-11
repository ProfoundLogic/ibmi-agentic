     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE Work with ICD9 codes
     H****************************************************************
     H*  Revisions:                                                  *
     H*                                                              *  *
     H*  05/20/09 C:33 Project: WC1770                               *  *
     H*                This program will now only call the system    *  *
     H*                 version.                                     *  *
     H*                                                              *  *
     H*  02/19/01 C:62 Project:FSMNT                                 *  *
     H*                FIX THE SUBSET OF LOST.                       *  *
     H*                                                              *  *
     H*  10/10/00 C:79 Project:MC025/24609                           *  *
     H*                Add logic on LOST subset to use logicals in   *  *
     H*                LOST order                                    *  *
     H*                                                              *  *
     H*  09/11/00 C:62 PROJECT:MC205                                 *  *
     H*                ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.*  *
     H*                                                              *  *
     H*  05/25/00 C:62 PROJECT:MC205                                 *  *
     H*                ADD F21=PRINT LIST                            *  *
     H*                                                              *
     H*  03/08/00 C:62 PROJECT: MCMNT                                *
     H*                -ADD SUBSET FOR LOS TYPE.                     *
     H*                -AN ADDITIONAL ENTRY PARM HAS BEEN ADDED,     *
     H*                 THE PARM WILL BE USED TO INITIALIZE THE LOS  *
     H*                 TYPE SUBSET FIELD.                           *
     H*                                                              *
     H*  08/27/98 C:62 PROJECT: MC090 13205                          *
     H*                OPT20 SHOULD NOT EXECUTE THE SCREEN OF        *
     H*                F2010F01.                                     *
     H*                                                              *
     H*    Date   Cons  Project/Revision                             *
     H*  05/05/97 C:69  Project: MC150; developed display/program    *
     H*                                                              *
     H****************************************************************
     FF2010F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMDIP     IF   E             DISK
     F                                     RENAME(PDI01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMDIL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDI01:DBRCDF)
     F*
     FFMDIL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDI01:DBRCDF2)
     F*** BEG ADD *** 10/10/00 ****************************************
     FFMDIL3    IF   E           K DISK
     F*** BEGIN CHG *** 02/19/01 **************************************
     F**********                                    KINFDS DBFDS3
     F**********  PDI01                             KRENAMEDBRCDF3
     F                                     INFDS(DBFDSA)
     F                                     RENAME(PDI01:DBRCDFA)
     F*** END   CHG *** 02/19/01 **************************************
     FFMDIL4    IF   E           K DISK
     F*** BEGIN CHG *** 02/19/01 **************************************
     F**********                                    KINFDS DBFDS4
     F**********  PDI01                             KRENAMEDBRCDF4
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PDI01:DBRCDFB)
     F*** END   CHG *** 02/19/01 **************************************
     F*** END ADD *** 10/10/00 ****************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*** BEGIN ADD *** 02/19/01 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D*** END   ADD *** 02/19/01 **************************************
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ICD9 Codes')
     D ACDPGM          C                   CONST('F2010R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D*
     D DELETD          C                   CONST('Deleted')
     D*** BEG ADD *** 01/16/99 **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     I*** END ADD *** 01/16/99 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@ICD9
     C                   PARM                    @@LOST
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKDSC1
     C                   KFLD                    KKICD9
     C*** BEG ADD *** 10/10/00 ****************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    SSLOST
     C                   KFLD                    KKICD9
     C*
     C     KEY4          KLIST
     C                   KFLD                    SSLOST
     C                   KFLD                    KKDSC1
     C*
     C     KEY5          KLIST
     C                   KFLD                    SSLOST
     C*** END ADD *** 10/10/00 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DIICD9        @@ICD9
     C     *LIKE         DEFINE    DILOST        @@LOST
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    DIDSC1        KKDSC1
     C*** BEG ADD *** 01/16/99 *************************************
     C     *LIKE         DEFINE    DIDSC1        U1PDSC
     C     *LIKE         DEFINE    SSDESC        U2PDSC
     C*** END ADD *** 01/16/99 *************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*** BEGIN ADD *** 02/19/01 **************************************
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C*
     C     SSLOST        WHENNE    *BLANKS
     C   21              Z-ADD     DBRRNA        SCDBRR
     C   22              Z-ADD     DBRRNB        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END   ADD *** 02/19/01 **************************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
B01  C     SSDESC        IFNE      *BLANKS
B01  C     SSSTAT        ORNE      *BLANKS
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        ORNE      *BLANKS
     C     SSTOCD        ORNE      *BLANKS
     C*** END   ADD *** 05/25/00 **************************************
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     SSLOST        ORNE      *BLANKS
     C*** END   ADD *** 03/08/00 **************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C***  BEGIN ADD  ***  02/19/01  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  02/19/01  *********************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     SSLOST        IFNE      *BLANKS
     C     SSLOST        ANDNE     'D'
     C     SSLOST        ANDNE     'O'
     C*
     C                   SETON                                        8890
     C*
     C                   ENDIF
     C*** END   ADD *** 03/08/00 **************************************
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFGT      SSTOCD
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
     C*** BEGIN CHG *** 03/08/00 **************************************
     C********** SSSTAT    IFNE ' '
     C  N90SSSTAT        IFNE      ' '
     C*** END   CHG *** 03/08/00 **************************************
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C                   ENDSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* ERRCKP - Error checking for the prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     SPFRCD        IFGT      SPTOCD
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   MOVEL     'A'           SSSTAT
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     Q1PRMS        IFGT      *ZEROS
     C     @@LOST        IFNE      *BLANKS
     C*
     C                   MOVEL     @@LOST        SSLOST
     C                   MOVEL     *BLANKS       @@LOST
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/08/00 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        KKICD9
     C                   MOVEL     *LOVAL        KKLOST
     C                   MOVEL     *LOVAL        KKDSC1
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL     SCPST1        KKICD9
     C   21              MOVEL     *LOVAL        KKLOST
     C   22              MOVEL     SCPST2        KKDSC1
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C                   MOVEL     SCDSC1        KKDSC1
     C                   ENDSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   MOVEL(P)  *BLANKS       SPFRCD
     C                   MOVEL(P)  *BLANKS       SPTOCD
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   EXSR      ERRCKP
     C*
     C     *IN90         IFEQ      *OFF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   MOVE      ##CO#         ##CO
     C*
     C                   CALL      'F7020C'
     C                   PARM                    ##CO              3
     C*** BEGIN CHG *** 09/11/00 **************************************
     C**********           PARM           SSFRCD
     C**********           PARM           SSTOCD
     C                   PARM                    SPFRCD
     C                   PARM                    SPTOCD
     C*** END   CHG *** 09/11/00 **************************************
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 05/25/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 02/19/01 **************************************
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C                   ENDIF                                                  VLDOPT<>Y
     C***  END   ADD  ***  02/19/01  *********************************
     C                   MOVEL     DIICD9        SCICD9
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDSC1        DES40            40
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  DIICD9        SCDATA
     C                   MOVE      DES40         SCDATA
     C                   ENDIF
     C     *IN22         IFEQ      *ON
     C                   MOVEL(P)  DES40         SCDATA
     C                   MOVE      DIICD9        SCDATA
     C                   ENDIF
     C                   MOVEL     DILOST        SCLOST
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     DISTAT        SCSTAT
     C                   ENDSR
     C*================================================================
     C* OPT20 - Change status
     C*================================================================
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
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 05/25/00 **************************************
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*** BEGIN DEL *** 08/27/98 **************************************
     C********** SCSEL     IFEQ '20'
     C**********           MOVE 'S'       ##MODE
     C**********           CALL ACDPGM    ACDPL        99
     C**********           ENDIF
     C*** END   DEL *** 08/27/98 **************************************
     C*** BEGIN ADD *** 08/27/98 **************************************
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*** END   ADD *** 08/27/98 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C     *LIKE         DEFINE    @@CALL        ##CALL
     C     *LIKE         DEFINE    @@ICD9        ##ICD9
     C     *LIKE         DEFINE    @@LOST        ##LOST
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@CALL        ##CALL
     C                   ENDIF
     C     Q1PRMS        IFGE      2
     C                   MOVE      @@KC          ##KC
     C                   ENDIF
     C     Q1PRMS        IFGE      3
     C                   MOVE      @@KL          ##KL
     C                   ENDIF
     C     Q1PRMS        IFGE      4
     C                   MOVE      @@ICD9        ##ICD9
     C                   ENDIF
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@LOST        ##LOST
     C                   ENDIF
     C*
     C                   CALL      'S2010R'
     C                   PARM                    ##CALL
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    ##ICD9
     C                   PARM                    ##LOST
     C                   PARM      'MC'          ##AREA            2
     C*
     C     Q1PRMS        IFGE      4
     C                   MOVE      ##ICD9        @@ICD9
     C                   ENDIF
     C     Q1PRMS        IFGE      5
     C                   MOVE      ##LOST        @@LOST
     C                   ENDIF
     C*
     C                   EXSR      PGMEND
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C***  End   Add  ***  05/20/09  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C***** BEGIN ADD **** 02/19/01 ***********************************
     C                   MOVE      'N'           VLDOPT
     C***** END   ADD **** 02/19/01 ***********************************
     C*** BEG ADD *** 10/10/00 ****************************************
     C                   SELECT
     C*
     C     SSLOST        WHENNE    *BLANK
     C*
     C*** BEGIN CHG *** 02/19/01 **************************************
     C***21***** KEY5      READEDBRCDF3                  95
     C***22***** KEY5      READEDBRCDF4                  95
     C   21KEY5          READE     DBRCDFA                                95
     C   22KEY5          READE     DBRCDFB                                95
     C*** END   CHG *** 02/19/01 **************************************
     C*
     C                   OTHER
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C*
     C*** BEG ADD *** 10/10/00 ****************************************
     C                   ENDSL
     C*** END ADD *** 10/10/00 ****************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C***** BEGIN ADD **** 02/19/01 ***********************************
     C                   MOVE      'N'           VLDOPT
     C***** END   ADD **** 02/19/01 ***********************************
     C*** BEG ADD *** 10/10/00 ****************************************
     C*
     C                   SELECT
     C*
     C     SSLOST        WHENNE    *BLANK
     C*
     C*** BEGIN CHG *** 02/19/01 **************************************
     C***21***** KEY5      REDPEDBRCDF3                  95
     C***22***** KEY5      REDPEDBRCDF4                  95
     C   21KEY5          READPE    DBRCDFA                                95
     C   22KEY5          READPE    DBRCDFB                                95
     C*** END   CHG *** 02/19/01 **************************************
     C*
     C                   OTHER
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C*** BEG ADD *** 10/10/00 ****************************************
     C*
     C                   ENDSL
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C*** BEG ADD *** 10/10/00 ****************************************
     C*
     C                   SELECT
     C*
     C     SSLOST        WHENNE    *BLANK
     C*
     C*** BEGIN CHG *** 02/19/01 **************************************
     C***21***** KEY3      SETGTDBRCDF3              96
     C***22***** KEY4      SETGTDBRCDF4              96
     C   21KEY3          SETGT     DBRCDFA                            96
     C   22KEY4          SETGT     DBRCDFB                            96
     C*** END   CHG *** 02/19/01 **************************************
     C*
     C                   OTHER
     C*** END ADD *** 10/10/00 ****************************************
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C*
     C*** BEG ADD *** 10/10/00 ****************************************
     C                   ENDSL
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*** BEG ADD *** 10/10/00 ****************************************
     C*
     C                   SELECT
     C*
     C     SSLOST        WHENNE    *BLANK
     C*
     C*** BEGIN CHG *** 02/19/01 **************************************
     C***21***** KEY3      SETLLDBRCDF3              96
     C***22***** KEY4      SETLLDBRCDF4              96
     C   21KEY3          SETLL     DBRCDFA                            96
     C   22KEY4          SETLL     DBRCDFB                            96
     C*** END   CHG *** 02/19/01 **************************************
     C*
     C                   OTHER
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C*
     C*** BEG ADD *** 10/10/00 ****************************************
     C                   ENDSL
     C*** END ADD *** 10/10/00 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDSC1
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   MOVEL     SCICD9        @@ICD9
     C                   MOVEL     SCLOST        @@LOST
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DISTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
B01  C     SSDESC        IFNE      *BLANKS
     C*** BEG ADD *** 01/16/99 *************************************
     C     LO:HI         XLATE(P)  DIDSC1        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*** END ADD *** 01/16/99 *************************************
     C                   CALL      'QCLSCAN'
     C***********          PARM           DIDSC1           DEL 01/16/99
     C                   PARM                    U1PDSC                         ADD 01/16/99
     C                   PARM      50            STRLEN            3 0
     C                   PARM      1             START             3 0
     C************         PARM           SSDESC           DEL 01/16/99
     C                   PARM                    U2PDSC                         ADD 01/16/99
     C                   PARM      35            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF
E01  C                   ENDIF
     C*** BEGIN ADD *** 03/08/00 **************************************
     C     SSLOST        IFNE      *BLANKS
     C     SSLOST        ANDNE     DILOST
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 03/08/00 **************************************
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     SSFRCD        IFNE      *BLANKS
     C     SSFRCD        ANDGT     DIICD9
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSTOCD        IFNE      *BLANKS
     C     SSTOCD        ANDLT     DIICD9
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/00 **************************************
     C                   ENDSR
** Column headings
Opt  Code  Description                               Type  Full code   Status
Opt  Description                              Code   Type  Full code   Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
