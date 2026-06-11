     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H** ========================================================== **
     H** ========================================================== **
     H*  NARRATIVE: Select various Groups from Subfile, then build   *
     H*             workfile of selected groups to process into a    *
     H*             single mbr IRS Tape/Electronic submission File    *
     H*  Created:                                                     *
     H*  12/12/97  C:69  Project: 60ANM                               *
     H** ========================================================== **
     H*  CALLED FROM:                                                *
     H*         MENU                                                 *
     H*  CALLS TO:                                                   *
     H*         W1099RT - Create Workfile 'T' Record for IRS File    *
     H*                   Add sequence nbrs to all file records      *
     H*                                                              *
     H*         W1099C3 - Create Combined Workfile and copy to Tape  *
     H*                   if tape device indicated on input screen   *
     H** ========================================================== **
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  04/30/19 C:062 Project: 152WAB                              *
     H*                 Apply Optacomp Mods.                         *
     H*                                                              *
     H*  09/11/15  C:295 Project: WC2362                             *
     H*                  To Pass on to group selection process       *
     H*                                                              *
     H*  03/12/13  C:282 PROJECT 147WAL                              *
     H*                  Group Selection for 1099 Process            *
     H*                                                              *
     H*  02/14/13  C:132 SUPPORT WC099    SEQ: 81682                 *
     H*                  1. Fix parameter mismatch when calling      *
     H*                     WA099C due to missing code.              *
     H*                                                              *
     H*                  2. Add missing changes from prior versions  *
     H*                     for support 51197.                       *
     H*                                                              *
     H*                  3. Correct changes done by 51197 to apply   *
     H*                     validation to included/selected groups   *
     H*                     and not to excluded/omitted groups.      *
     H*                                                              *
     H*                  4. Fix specs out of sequence                *
     H*
     H*  01/30/03  C:132 PROJECT: WC091   SEQ: 35412                 *
     H*                  Correct commented Code to check for not     *
     H*                  Same TCC codes                              *
     H*                                                              *
     H*  01/08/03  C:132 PROJECT: WC082   SEQ: 35412                 *
     H*                  Change Selection option from 1 to 20.       *
     H*                  Change Submit option from F6 to F7          *
     H*                                                              *
     H*  11/18/02  C:132 PROJECT: WC1096                             *
     H*                  Change to show all groups on initial screen *
     H*                  as "Exclude" to be consistant with W1099R1  *
     H*                  If not selected and is "Include" will then  *
     H*                  back to "Exclude".                           *
     H*                                                               *
     H*  10/11/02  C:132 PROJECT: WC1097                              *
     H*                  Change to allow the tape device to be blank *
     H*                  and pass a Parameter into W1099RT as a code  *
     H*                  to signify whether have a tape device or not *
     H*                                                               *
     H*  12/01/98  C:33  PROJECT: WC871                               *
     H*                  ADD CALL TO W1099RT TO WRITE TRANSMITTAL 'T' *
     H*                  RECORD BEFORE PROCESSING TAPE                *
     H*                                                               *
     H*  02/13/98  C:69  Project: 60ANM                               *
     H*                  Commented out the edit for the transmital    *
     H*                  codes having to be the same.                 *
     H*****************************************************************
     FW1099F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     INFDS(FIDS01)
     FWXTNP     O    E             DISK    USROPN
     F*
     FWDF2L     IF   E           K DISK
     F*  Physical file definition (for refreshing subfile record)
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFN01:DBRCDF)
     /copy sPrRuncmd
     /copy sPrMessage
     /copy sPrWrkwth
     /copy sPrString
      /COPY SCOPYWW01D
     D****** Beg Add ***** 09/11/15 **********************************
     D GroupSelect     pr                  extpgm('W7944C')
     D  Program                      10    const
     D****** End Add ***** 09/11/15 **********************************
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(6) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(6) ALT(OF)
      ***** BEG ADD **** 03/12/13 *************************************
     d charCltId       s              3
     d charUser        s             10
     d charAppl        s              1
     d charRtcd        s              1
     d charAuth        s              1
     d charGrp         s              3  0
      ***** End ADD **** 03/12/13 *************************************
     D*
     D*
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

      ***** BEG ADD **** 03/12/13 *************************************
     dRtvClientId      pr                  Extpgm('RTVCLTID')
     d charCltId                      3
      *
     dRtvGrpAuth       pr                  Extpgm('SA008R')
     d ##co#                          3  0 const
     d charUser                      10    const
     d charAppl                       1    const
     d charRtcd                       1
     d charAuth                       1          options(*nopass)
     d charGrp                        3  0 const options(*nopass)
      ***** End ADD **** 03/12/13 *************************************



     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Groups for 1099 Tape')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(999)
     D*
     D CLRF            C                   CONST('CLRPFM WXTNP')
      *
     D NOTAP1          C                   CONST('NOTAPE    ')
     D NOTAP2          C                   CONST('NO TAPE   ')

      ***** Beg Add **** 09/11/15 *************************************
      ***** BEG ADD **** 02/14/13 *************************************
     D***** /COPY SCOPYWW11C
      ***** END ADD **** 02/14/13 *************************************
      ***** End Add **** 09/11/15 *************************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
      ***** Beg Add **** 09/11/15 *************************************
      * Call to Group select program to pass-through
      /Free

        //*** Begin Chg *** 01/25/21 ***************************************
        //*** GroupSelect('1099RIRS');
        If @@call = ' ';
           GroupSelect('1099RIRS');
        Else;
           GroupSelect('1099RNEC');
        EndIf;
        //*** End   Chg *** 01/25/21 ***************************************
        Return;
      /End-Free

     C***** /COPY SCOPYWW11C
      ***** End Add **** 09/11/15 *************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     F2KEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FNFND         KKFND
     C*
     C***** BEG DELETE *******  02/14/13 *********************************
     C/COPY SCOPYWW11C
     C***** END DELETE *******  02/14/13 *********************************
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCKC - Errors on subfile control record
     C*================================================================
     C     ERRCKC        BEGSR
      *
      * If screen tape device is not blamk check for valid
      *
     C     SCTAPD        IFEQ      NOTAP2                                       SCTAPD=NO TAPE
     C                   MOVEL(P)  NOTAP1        SCTAPD                         SCTAPD<NOTAPE
     C                   ENDIF
      *
     C     SCTAPD        IFNE      *BLANKS
     C     SCTAPD        ANDNE     NOTAP1                                       SCTAPD=NOTAPE
     C                   MOVE      'Y'           ##TAPD            1
     C                   CALL      'SRCHKOBJ'
     C                   PARM      SCTAPD        ##OBJN           10
     C                   PARM      '*DEVD'       ##OBJT            7
     C     *IN90         PARM                    ##ERR             1
B01  C     *IN90         IFEQ      *ON
     C                   SETON                                        89
E01  C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      'N'           ##TAPD
     C                   ENDIF
      *
     C*
     C*  Check all transmittal codes to be the same
     C*
B01  C     *IN90         IFEQ      *OFF
     C     *INKG         ANDEQ     *ON
      * This commented out code is needed to check for same TCC number
     C**                   EXSR XCHKTC
     C                   EXSR      XCHKTC
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      ***** BEG CHG ***** 02/14/13 ***************************************
B01  C**** SCSTAT        IFEQ      'Exclude'
B01  C     SCSTAT        IFEQ      'Include'
      ***** END CHG ***** 02/14/13 ***************************************

B02  C     SCTCC         IFEQ      *BLANKS
     C                   SETON                                        7190
E02  C                   ENDIF

      ***** BEG DEL ***** 02/14/13 ***************************************
B02  C**** *IN90         IFEQ      *OFF
     C**** SCMCD         ANDLT     CUR180
     C****               SETON                                        7290
E02  C****               ENDIF
      ***** END DEL ***** 02/14/13 ***************************************

      ***** BEG ADD ***** 02/14/13 ***************************************
     C     *IN90         IFEQ      *OFF
     C                   MOVE      ##CO#         ##CO#2
     C                   MOVE      SCFND         ##FND2
     C                   MOVE      *ZEROS        XXCHGD            7 0
     C                   MOVE      *ZEROS        XXNRCD           10 0
     C                   CALL      'WA099C'
     C                   PARM                    ##CO#2            3
     C                   PARM                    ##FND2            3
     C                   PARM      *ZEROS        ##CHGD            7
     C                   PARM      *ZEROS        ##NRCD           10 0
     C                   ENDIF                                                  *IN90 = *OFF

     C                   MOVE      ##CHGD        XXCHGD
     C                   MOVE      ##NRCD        XXNRCD

      * If dont get a change date, member is missing.
     C     *IN90         IFEQ      *OFF
     C     XXCHGD        ANDEQ     *ZEROS
     C                   SETON                                        7290
     C                   ENDIF

      * Check member information is current.
     C     *IN90         IFEQ      *OFF
     C     XXCHGD        ANDLT     CUR180
     C     XXNRCD        ANDGT     *ZEROS
     C                   SETON                                        7290
     C                   ENDIF
      ***** END ADD ***** 02/14/13 ***************************************
     C*
E01  C                   ENDIF
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
     C                   MOVE      *LOVAL        KKFND
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKFND
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCFND         KKFND
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNDS
     C                   MOVE      *BLANKS       SCTCC
     C     F2KEY         CHAIN     WDF2L                              99
B01  C     *IN99         IFEQ      *OFF
     C                   MOVE      F2TCC         SCTCC
E01  C                   ENDIF
      ********* BEG DEL ********** 02/14/13 *****************************
     C***                MOVE      *ZEROS        SCMCD
     C***                MOVE      ##CO#         ##CO#2
     C***                MOVE      FNFND         ##FND2
     C***                CALL      'WA099C'
     C***                PARM                    ##CO#2            3
     C***                PARM                    ##FND2            3
     C***                PARM                    ##CHGD            7
     C***                MOVE      ##CHGD        SCMCD
      ********* END DEL ********** 02/14/13 *****************************
     C*
B01  C     SCSEL         IFNE      '20'
     C*
     C*  Check for member in WDTAP file
     C                   MOVE      'Exclude'     SCSTAT
     C                   ENDIF
     C*
     C*  Chain to group aux to check for Transmitter code
     C*
     C     SCSTAT        IFEQ      'Include'
     C     SCTCC         IFEQ      *BLANKS
     C                   MOVE      'Exclude'     SCSTAT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   EXSR      ERRCKC
B01  C     *IN90         IFEQ      *OFF
     C*
B02  C     *INKG         IFEQ      *ON
     C*
     C*  Clear WXTNP
     C*
     C                   CALL      'QCMDEXC'                            90
     C                   PARM      CLRF          ##CMD
     C                   PARM      12            ##CMDL
E02  C                   ENDIF
     C*
B02  C     *IN90         IFEQ      *ON
     C                   SETON                                        88
X02  C                   ELSE
     C*
     C*  Open WXTNP, write records from subfile
     C*
     C                   OPEN      WXTNP
B03  C     1             DO        LASTRR        IX                5 0
     C     IX            CHAIN     SCR01S                             99
B04  C     *IN99         IFEQ      *OFF
B05  C     SCSTAT        IFEQ      'Include'
     C                   MOVEL     ##CO#         TNCO#
     C                   MOVEL     SCFND         TNGRP
     C                   WRITE     PTN01
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDDO
     C*
     C                   CLOSE     WXTNP
     C*
      * Setup return parms for Co Count and Last sequence Nbr
     C                   MOVE      *BLANKS       ##CCNT            8
     C                   MOVE      *BLANKS       ##SEQN            8
     C*
     C*** Begin Add *** 01/25/21 *******************************************
     C                   If        @@call = ' '
     C*** End   Add *** 01/25/21 *******************************************
     C                   CALL      'W1099RT'                            99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM                    ##TAPD
     C                   PARM                    ##CCNT            8
     C                   PARM                    ##SEQN            8
     C*** Begin Add *** 01/25/21 *******************************************
     C                   Else
     C                   CALL      'W1099RTNEC'                         99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM                    ##TAPD
     C                   PARM                    ##CCNT            8
     C                   PARM                    ##SEQN            8
     C                   Endif
     C*** End   Add *** 01/25/21 *******************************************
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   OPEN      WXTNP
     C*
     C*  Call program to submit job to batch
     C*
     C                   CALL      'W1099C3'
     C                   PARM                    SCTAPD
      * Pass return parms for Co Count & Last seq nbr into CL
     C                   PARM                    ##CCNT            8
     C                   PARM                    ##SEQN            8
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ELSE
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKC=*OFF=*INKL
     C*
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
B01  C     SCSEL         IFEQ      '20'
B02  C                   SELECT
S01  C     SCSTAT        WHENEQ    'Include'
     C                   MOVE      'Exclude'     SCSTAT
S02  C     SCSTAT        WHENEQ    'Exclude'
     C                   MOVE      'Include'     SCSTAT
E02  C                   ENDSL
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      **** Begin add 02/14/13 ****************************************
     C                   MOVE      *ON           *INLR
      **** End   add 02/14/13 ****************************************

     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        ##DTIN
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DTIN            8 0
     C                   PARM      180           ##NDYS            5 0
     C                   PARM                    ##ERR             1
     C*
     C                   Z-ADD     ##DTIN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CUR180            7 0
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21##CO#         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21##CO#         READPE    DBRCDF                                 95
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
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      ********* Beg Add ********** 03/12/13 *****************************
      /Free
       RtvClientId(charCltId);
       //*** Begin Chg *** 04/30/19 *************************************
       //*** If charCltId = '114';
       If charCltId = '114' Or charCltId = '152';
       //*** End   Chg *** 04/30/19 *************************************
          ExSr RtvGrpAuthority;
          If charRtcd = *On;
             meetss = *off;
          EndIf;
       EndIf;
      /End-Free
      ********* End Add ********** 03/12/13 *****************************
     C                   ENDSR
     C*================================================================
     C* XCHKTC - Check transmital codes to be the same
     C*================================================================
     C     XCHKTC        BEGSR
     C     *LIKE         DEFINE    SCTCC         SVTCC
     C                   MOVE      *BLANKS       SVTCC
B01  C     1             DO        LASTRR        IX                5 0
     C     IX            CHAIN     SCR01S                             99
B02  C     *IN99         IFEQ      *OFF
B03  C     SCSTAT        IFEQ      'Include'
B04  C     SVTCC         IFEQ      *BLANKS
     C                   MOVE      SCTCC         SVTCC
E04  C                   ENDIF
B04  C     SCTCC         IFNE      SVTCC
     C                   SETON                                        9073
     C                   SETON                                        0507
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   UPDATE    SCR01S
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E01  C  N90              ENDDO
     C                   ENDSR
      ********* Beg Add ********** 03/12/13 *****************************
     C*================================================================
     C* RTVGRPAUTH - Retrieve Group Authority
     C*================================================================
      /Free

        BegSr RtvGrpAuthority;
        charUser = Q1user;
        charAppl = 'W';
        charRtcd = *off;
        charGrp  = FNfnd;
        RtvGrpAuth(##co#:charUser:charAppl:charRtcd:charAuth:charGrp);

        EndSr;
      /End-Free
      ********* End Add ********** 03/12/13 *****************************
** Column headings
Opt Group Description                               Status   Trans ID
** Description--------------MISN
12020=Include/Exclude       YYN
203F3=Exit                  YYN
205F5=Refresh               NNN
207F7=Submit job            YNN
213F13=Chg user parms       YYY
3
