     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* PROGRAM: F2380R                                              *
     H* DESCRIPTION: SELECT GENERIC TABLE ITEMS
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*
     H*  11/14/12 C:287 Project: MC367
     H*                 Removed option 5 and function 17 for GS
     H*                                                              *
     H* 02/01/00 C:53  KY010
     H*                ADD CHANGE VIEW FUNCTIONALITY
     H*
     FF2380F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTGTL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PGT01:DBRCDF)
     F*** BEG ADD *** 02/01/00 ******************************
     F*  Secondary logical file definition (for displaying items)
     FFTGTL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PGT01:DBRCDF2)
     F*** END ADD *** 02/01/00 ******************************
     FFTGIL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D DTA1            S              1    DIM(52)                              ADD 02/01/00
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
     D ACDPGM          C                   CONST('F2380R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D*** BEG CHG *** 02/01/00 ******************************
     D VIEWMX          C                   CONST(2)

     I*** END CHG *** 02/01/00 ******************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@TBID
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
     C*** BEG ADD *** 02/01/00 ******************************
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKDESC
     C*** END ADD *** 02/01/00 ******************************
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
     C     *LIKE         DEFINE    GTDESC        KKDESC                         ADD 02/01/00
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
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
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
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
     C**************       MOVELSCPST1    KKKEY            CHG 02/01/00
     C   21              MOVEL     SCPST1        KKKEY                          CHG 02/01/00
     C   22              MOVEL(P)  SCPST2        KKDESC                         ADD 02/01/00
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCKEY         KKKEY
     C                   MOVEL(P)  SCDESC        KKDESC                         ADD 02/01/00
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
     C                   MOVEL     GIDESC        SCTBNM
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     GTKEY         SCKEY
     C                   MOVEL     GTSTAT        SCSTAT
     C                   MOVEL     GTDESC        SCDESC
     C*
     C                   SELECT                                                 ADD 02/01/00
     C     HD#           WHENEQ    1                                            ADD 02/01/00
     C                   MOVEL(P)  GTKEY         SCDATA
     C                   MOVE      GTDESC        SCDATA
     C*** BEG ADD *** 02/01/00 ******************************
     C     HD#           WHENEQ    2
     C                   MOVEA     GTDESC        DTA1(1)
     C                   Z-ADD     45            Y                 2 0
     C                   MOVEA     GTKEY         DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C                   MOVEL     GTSTAT        SCSTAT
     C                   ENDSL
     C*** BEG ADD *** 02/01/00 ******************************
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
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
     C*** Beg Add *** MC367 *** 11/15/12 *****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 *****************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95    ADD 02/01/00
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95    ADD 02/01/00
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96        ADD 02/01/00
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96        ADD 02/01/00
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
     C*
     C                   ENDSR
** Column headings
Code        Description                                               Sts
Description                                 Code                      Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
