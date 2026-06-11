     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H*    CREATED 01/26/98   C:86   PROJECT DR052
     H*
     H*****************************************************************
     H*                                                              *
     H** REVISIONS:                                                 **
     H*                                                              *
     H*  01/26/04 C:62  PROJECT: PR029                               *
     H*                 ADDED ADDITIONAL PARM TO SUBSET ON TYPE      *
     H*                                                              *
     H*  06/11/98 C:94  PROJECT: DR052                               *
     H*                 ADDED A DESCRIPTION SEARCH TO SCREEN 3.      *
     H*                                                              *
     H****************************************************************
     FS2380F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTGTL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PGT01:DBRCDF)
     FSTGIL     IF   E           K DISK
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
     D OPTNM           C                   CONST('Generic Table Items')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('S2380R01')
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
     C                   PARM                    @@TBID
     C*** BEGIN ADD *** 01/26/04 **************************************
     C                   PARM                    @@TYPE            1
     C*** END   ADD *** 01/26/04 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@TBID        #@TBID
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
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKKEY
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        #@TBID
     C     *LIKE         DEFINE    GTTBID        @@TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** BEG ADD *** 06/11/98 **************************************
     C     *LIKE         DEFINE    SCDSCR        U1PDSC
     C     *LIKE         DEFINE    SCDSCR        U2PDSC
     C*** END ADD *** 06/11/98 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*** BEG ADD *** 06/11/98 **************************************
     C     SSDESC        IFNE      *BLANKS
B01  C     SSSTAT        ORNE      *BLANKS
     C*** END ADD *** 06/11/98 **************************************
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
     C                   MOVEL     @@TBID        ##TBID
     C                   MOVE      *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C                   ENDSR
     C*================================================================
     C* MKTBHD - Makes the table id header.
     C*================================================================
     C     MKTBHD        BEGSR
     C*
     C                   MOVEL     @@TBID        ##TBID
     C                   MOVEL     @@TBID        SCTBID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C*
     C                   MOVEL     GIDESC        SCTBNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     GTKEY         SCKEY
     C*
     C                   MOVEL(P)  GTKEY         SCKY
     C                   MOVEL(P)  GTDESC        SCDSCR
     C*
     C                   MOVEL     GTSTAT        SCSTAT
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
     C                   MOVE      *ON           *INLR
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
     C* EXECUTE SUBROUTINE TO CREATE TABLE ID HEADER
     C*
     C                   EXSR      MKTBHD
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
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
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     GTSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEG ADD *** 06/11/98 ************************************
     C*
     C********** SCDSCR    IFNE *BLANKS
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  GTDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
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
     C                   ENDIF                                                  SCNRES = 0
     C                   ENDIF                                                  SCDESC NE BL
     C*
     C*** END ADD *** 06/11/98 ************************************
     C*** BEGIN ADD *** 01/26/04 ****************************************
     C     Q1PRMS        IFGT      5
     C     @@TYPE        ANDNE     *BLANKS
     C     @@TYPE        IFNE      GTFG01
     C     @@TYPE        ANDNE     GTFG02
     C     @@TYPE        ANDNE     GTFG03
     C     @@TYPE        ANDNE     GTFG04
     C     @@TYPE        ANDNE     GTFG05
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/26/04 ****************************************
     C*
     C                   ENDSR
** Column headings
Code         Description                                              Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
