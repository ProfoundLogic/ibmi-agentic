     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H BndDir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W0815R - WORK WITH WAIVERS
      *****************************************************************
      *  DESCRIPTION:  This program will display a list of all        *
      *                certificate holders for a selected employer    *
      *                policy year.  Each holder can be accessed for  *
      *                update, display or selection depending on the  *
      *                calling program.                               *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0050R -> W0051R1 -> W0051R24 -> W00815R                   *
      *                  AND                                          *
      *    W0051R (OPT54) -> W1054R -> W1054R02 -> W0815R             *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *                                                               *
      *  CREATED: 05/12/99  PROGRAMMER: C:113 PROJECT: CM045          *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 08/12/25 C:333 Support: 119491 (FCC)                          *
      *                Edit Existing Waiver of Subrogation Record.    *
      *                                                               *
      * 08/12/22 C:079 Support: 148/93027                             *
      *                Map the old data from WXFMP1 into WMWVP        *
      *                                                               *
      * 02/02/21 C:136 Project: WC2757                                *
      *                Automate write of WMWVP for TX blanket waivers.*
      *                                                               *
      * 02/12/20 C:136 Support: 80310                                 *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *                Display only waiver detail for the form being  *
      *                processed.                                     *
      *                                                               *
      * 11/21/19 C:136 Support: 84697                                 *
      *                Discontinue use of LDSEQ#                      *
      *                                                               *
      * 10/24/17 C:267 Project C150WBP                                *
      *               Form Added WC990313.                           *
      *                Add call to SRCHKPGM                           *
      *                                                               *
      * 08/04/16 C:302 Project PHW42654                               *
      *                DELETION FOR WOS ENDORSMENT.                   *
      *                                                               *
      * 04/03/16 C:302 Project WC2409                                 *
      *                Allow waivers across divisions.                *
      *                                                               *
      * 11/25/15 C:302 Project WC2217                                 *
      *                The Waiver Endorsement process is being        *
      *                updated to print all wavier records on         *
      *                a policy.                                      *
      *                                                               *
      * 10/19/14 C:302 Support 86786                                  *
      *               Form Added WC420304B.                          *
      *                                                               *
      * 10/02/13 C:287 Project WC2194                                 *
      *               Added waiver state condition for Texas endors. *
      *                                                               *
      * 10/30/09 C:190 SUPPORT 70020                                  *
      *                Change options for CLT045                      *
      *                                                               *
      * 08/28/08 C:197 SUPPORT 64079                                  *
      *                NEED TO PROVIDE A MEANS FOR COMPWEST TO DELETE *
      *                WAIVERS THAT HAVE BEEN ENTERED IN ERROR        *
      *                                                               *
      * 07/23/08 C:197 PROJECT 132WKN                                 *
      *                ENABLE ENTRY OF WAIVER INFORMATION FOR NEW     *
      *                ENDORSEMENT WC990306.                          *
      *                                                               *
      * 07/16/07 C:183 PROJECT 45WFO                                  *
      *                Revert back changes for 45WBZ                  *
      *                                                               *
      * 09/14/06 C:61  SUPPORT WA133 - 52938                          *
      *                Change/Delete options only for CLT133          *
      *                                                               *
      * 07/21/06 C:33  Project: 45WFB                                 *
      *                Implement 45WBZ for Back-to-Base.              *
      *                                                               *
      * 06/28/06 C:132 PROJECT WA133 - 52938                          *
      *                Re-instate Change/Remove options for Waiver    *
      *                endorsements for all.                          *
      *                                                               *
      * 06/11/06 C:61  PROJECT WC1333                                 *
      *                Change to allow processing of CA Waiver        *
      *                endorsements.  (end# = 0).                     *
      *                                                               *
      * 04/26/06 C:61  PROJECT 132WFI                                 *
      *              - Add entry parmeter to indicate mid-term        *
      *                waiver type (ADD, CHANGE, DELETE).             *
      *              - Add parmeter in calling Maintenance W0815R02   *
      *                to indicate (ADD, CHANGE, DELETE).             *
      *              - Change which functions/options available       *
      *                depending upon with type of mid-term waiver    *
      *                is being processed.                            *
      *                                                               *
      * 04/03/06 C:61  SUPPORT WA109 51864                            *
      *              - Remove subset by endorsement #.                *
      *              - Do not allow options 2 and 4, or function 6    *
      *                when called from WAIVER INFORMATION.           *
      *              - Do not allow options 2 or 4 on mid term waiver *
      *                records when called from attach program        *
      *                (SR00313).                                     *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *

     FW0815F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *  Physical file definition (for refreshing subfile record)
     F*** Beg Chg *** 08/19/22 ************************************************
     F***WMWVP     IF   E             DISK
     FWMWVP     IF A E             DISK
     F*** End Chg *** 08/19/22 ************************************************
     F                                     RENAME(PWV01:PFRCDF)
      *  Primary logical file definition (for displaying items)
     FWMWVL8    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PWV01:DBRCDF)
     FWMWVL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PWV01:DBRCDF2)
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     fwmwvl9    if   e           k disk    rename(pwv01:pwv09) infds(wvl9DS)
     fwtenl     if   e           k disk    rename(pen01:pen00)
     f*** Beg Add *** 08/12/22/ ***********************************************
     fwxfmp1    uf   e           k disk
     f*** End Add *** 08/12/22/ ***********************************************

     d wvl9DS          ds
     d  binL9RRN             397    400b 0
      /COPY SCOPYWW01D
      *================================================================
      *  Array & Table definitions
      *================================================================
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *================================================================
      *  Data Structures & constants definitions
      *================================================================
     D OPTNM           C                   CONST('Waivers of Subrogati-
     D                                     on')
     D DELETD          C                   CONST('*Deleted')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0815R02')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(8)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SCDATA                 1     80
     D  XXDATA                 1     47
     D  XXPROJ                50     80
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0

     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     d  ldWvrMode             32     32
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55

     d chrAddPgm       s                   Like(WVpgm)
     d chrPrtPgm       s                   Like(WVpgm)
     d chrState        s                   Like(WVemst)
     d chrWvBlkPrjDs   s                   Like(WvBlkPrjDs)
     d chrWvrForm      s                   Like(WVenm)
     d chrWvrType      s              1
     d isBlkWvrType    s               n
     d i1              s              3  0
     d xxNam1          s             30
     d*** Beg Add *** 08/12/22 *********************************************
       dcl-s decWvr# Like(WVwav#);
     d*** End Add *** 08/12/22 *********************************************


     d W0815R02        pr                  ExtPgm('W0815R02')
     d  ##kc                          1
     d  ##kl                          1
     d  ##mode                        1    Const
     d  ##rrn                         9  0
     d  ##rrn2                        9  0
     d  ##WTyp                        1
     d  ##CPgm                       10

      /copy wPrEmplr
      /copy wPrEndorse
      /copy sCopyWW01c
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@CPGM           10
     C                   PARM                    @@WTYP            1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *  Add/change/display program parameter list
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      XXWTYP        ##WTYP            1
     C                   PARM      XXCPGM        ##CPGM           10
      *  Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *  Agent/Producer retrieve program parameter list
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *  Change Numeric to Alpha program parameter list
     C
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *================================================================
      *  KLIST definitions
      *================================================================
      *  EMPLOYER MASTER
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *  GROUP MASTER
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND

     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#

     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    KKWAV#

     C     KEY1B         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    KKNAME

      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    WVCO#         KKCO#
     C     *LIKE         DEFINE    WVFND         KKFND
     C     *LIKE         DEFINE    WVFYR         KKFYR
     C     *LIKE         DEFINE    WVEMP#        KKEMP#
     C     *LIKE         DEFINE    WVDIV         KKDIV
     C     *LIKE         DEFINE    WVWAV#        KKWAV#
     C     *LIKE         DEFINE    WVNAME        KKNAME
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    SCCXDT        SYSDT1
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
       Begsr ErrCk1;
        // Can't change or delete midterm endorsement records
        //  unless you want to.
        If Q1prms < 7;
           If cltId = '133' Or
            cltId = '132' Or
            cltId = '045' Or
            cltId = '150';
              LeaveSr;
           EndIf;
           If scEnd# <> 0 And (scSel = '2' Or scSel = '4');
              *In71 = *On;
              *In90 = *On;
           Endif;
        EndIf;
       EndSr;
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKWAV#
     C                   MOVE      *LOVAL        KKNAME
     C                   ENDSR
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKWAV#
     C                   MOVE      SCPST2        KKNAME
     C                   ENDSR
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCWAV#        KKWAV#
     C                   MOVEL     SCNAM1        KKNAME
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR

      *** Beg Chg ***** 08/12/25 *****************************
     C***                Z-ADD     WVEND#        SCEND#
     C                   Z-ADD     LDEND#        SCEND#
      *** End Chg ***** 08/12/25 *****************************
     C                   Z-ADD     WVWAV#        SCWAV#
     C                   MOVEL     WVNAME        SCNAM1
     C                   MOVEL     WVDIV         SCDIV

     C                   MOVEL(P)  WVWAV#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XXWAV#            5
     C                   MOVEL(P)  WVDIV         XXDIV1            5
       If WVname <> *Blanks;
          xxNam1 = WVname;
       Else;
          xxNam1 = 'Blanket Waiver';
       Endif;
       If WVblkPrjDS <> *Blanks;
          xxProj = WVblkPrjDs;
       Else;
          xxProj = WVproj;
       Endif;

     C                   CLEAR                   XXDATA

     C                   SELECT

     C     VIEW#         WHENEQ    1

     C                   MOVEL     XXWAV#        XXWVDV           12
     C                   MOVE      XXDIV1        XXWVDV
     C                   MOVEL     XXWVDV        XXDATA
     C                   MOVE      XXNAM1        XXDATA

     C     VIEW#         WHENEQ    2

     C                   MOVEL     XXDIV1        XXWVDV
     C                   MOVE      XXWAV#        XXWVDV
     C                   MOVEL     XXNAM1        XXDATA
     C                   MOVE      XXWVDV        XXDATA

     C                   ENDSL

     C                   ENDSR
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR

     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

      *  Check for CA endorsement form
     C                   CALL      'SRCHKPGM'
     C                   PARM      'SR99313'     #1PGM            10
     C                   PARM      *BLANK        #SR99313          1

     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@RRN2        CHAIN     PEL01                              60

     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA

      /Free
       // Get 7th parm for Midterm Endorsement processing.
       //  @@WTyp will be Add, Change, or Delete waiver
       // @@Call is sent in by W/W Employer Endorsements through
       //  the endorsement program.
       xxWTyp = *Blanks;
       If Q1prms > 6;
          xxWTyp = @@Wtyp;
       Endif;
       // Bypass W0815R for state specific blanket waiver forms
       Chain (ELco#:ELfnd:ldWaiverFrm) pen00;
       If Enst <> *Blanks;
         // Find division for endorsement state. Edits from
         //  previous pgms have already determined that one exists.
          If GetWaiverInfo(ELco#:ELfnd:ENst:ELpfdt:ldWaiverFrm:
           chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ELfyr:ELemp#) = *On;
             isBlkWvrType = %Subst(chrWvBlkPrjDs:500:1) = 'B';
          Endif;
          If isBlkWvrType = *On;
             Chain (ELco#:ELfnd:ELfyr:ELemp#:ldWaiverFrm) pwv09;
             If %Found;
                ##RRN = binL9RRN;
             Endif;
             ##Mode = @@Call;
             ##RRN2 = @@RRN2;
             ##CPgm = @@CPgm;
             ##WTyp = xxWTyp;
             W0815R02(##kc:##kl:##Mode:##RRN:##RRN2:##WTyp:##Cpgm);
             Return;
          Endif;
       Endif;
       // Bypass W0815R for adding waivers or deleting a blanket
       //  waiver from midterm endorsements
       Select;
       When ldWvrMode = 'A'; // Called from W/W Employer Endorsements to Add Waiver
          ##Mode = ldWvrMode;
          ##RRN2 = @@RRN2;
          ##CPgm = @@CPgm;
          ##WTyp = xxWTyp;
          W0815R02(##kc:##kl:##Mode:##RRN:##RRN2:##WTyp:##Cpgm);
          Return;
       When xxWTyp = 'A';     // Called from Midterm Endorsements to Add Waiver
          ##Mode = @@Call;
          ##RRN2 = @@RRN2;
          ##CPgm = @@CPgm;
          ##WTyp = xxWTyp;
          W0815R02(##kc:##kl:##Mode:##RRN:##RRN2:##WTyp:##Cpgm);
          Return;
       When xxWTyp = 'D';     // Called from Midterm Endorsements to Dlt Waiver
          ##Mode = @@Call;
          ##RRN2 = @@RRN2;
          ##CPgm = @@CPgm;
          ##WTyp = xxWTyp;
         // Bypass W0815R if processing a blanket waiver
          If ELwvtp = 'B';
             Setll (ELco#:ELfnd:ELfyr:ELemp#:ldWaiverFrm) pwv09;
             Dou %Eof(WMWVL9);
                Reade (ELco#:ELfnd:ELfyr:ELemp#:ldWaiverFrm) pwv09;
                If %Eof;
                   Leave;
                Endif;
                If WVdiv = ELdiv;
                   Leave;
                Endif;
             Enddo;
             If Not(%Eof);
                ##RRN = binL9RRN;
                W0815R02(##kc:##kl:##Mode:##RRN:##RRN2:##WTyp:##Cpgm);
             Endif;
             Return;
          Endif;
       Endsl;
      /End-Free

     C                   Z-ADD     ELFYR         SCFYR

     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
      *  GET EMPLOYER MASTER RECORD
     C     EMKY          CHAIN     PEM01                              60

     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
      *  COMPRESS EMP#/DIV/NAME
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80

     C                   MOVEL     XXEDN         SCEDN

     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *  Get most recent cancellation date, if any
     C     *ZERO         LOOKUP    EF(I1)                                 61

     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON

     C     I1            IFNE      0

     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO

     C                   ENDIF                                                  I1 IFNE 0
      *  GET FUND MASTER INFORMATION
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME

     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT

     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGE     FNEXDT
     C                   MOVE      *ON           *IN09
     C                   MOVEL     '1'           @@CALL
     C                   ENDIF                                                  FNEXDT<>*ZEROS

     C                   Z-ADD     SYSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SYSDT1
      *  GET AGENT/PRODUCER INFORMATION
     C                   CALL      AGTAPI        AGTPL

     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *  GET USER PARM INFORMATION
     C     Q1USER        CHAIN     PUP01                              60

     C                   Z-ADD     1             VIEW#

     C     *LIKE         DEFINE    @@CPGM        XXCPGM

     C                   SELECT
     C     Q1PRMS        WHENGE    6
     C     @@CPGM        ANDNE     *BLANK
     C                   MOVE      @@CPGM        XXCPGM
     C                   OTHER
     C                   MOVE      *BLANK        XXCPGM
     C                   ENDSL

     C     Q1PRMS        IFGE      6
     C     #SR99313      ANDEQ     '1'
     C                   MOVEL     'SR99313   '  XXCPGM
     C                   ENDIF

      *  If called from WAIVER INFORMATION, do not allow
      *  options "2=Change", "4=Delete" or function "F6=Add"
     C     Q1PRMS        IFGE      6
     C     XXCPGM        ANDEQ     'W0815R01'

     C     CLTID         IFNE      '133'
     C     CLTID         ANDNE     '045'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CLTID <> 133 ,45

     C     CLTID         IFNE      '133'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CLTID <> 133
     C     CLTID         IFEQ      '132'

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CLTID <> 132

     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   ENDIF

     C     Q1PRMS        IFGE      7
     C     @@WTYP        ANDNE     *BLANK
     C                   MOVE      @@WTYP        XXWTYP            1
     C                   ELSE
     C                   MOVE      *BLANK        XXWTYP
     C                   ENDIF
      *  If called from MID-TERM OPTION
     C     Q1PRMS        IFGE      7
     C     XXWTYP        ANDNE     *BLANK
      *  ADD mid-term waiver
     C     XXWTYP        IFEQ      'A'

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   ENDIF                                                  ADD
      *  CHG mid-term waiver
     C     XXWTYP        IFEQ      'C'

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   ENDIF                                                  CHG
      *  DLT mid-term waiver
     C     XXWTYP        IFEQ      'D'

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '02'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '04'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA

     C                   ENDIF                                                  DLT

     C                   ENDIF                                                  Q1PRMS 7
       //*** Beg Add *** 08/12/22 *********************************************
       decWvr# = 1;
       Clear pfrcdf;
       Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pfm01;
       Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pfm01;
       Dow Not %Eof(WXFMP1);
          If F3ipgm = 'SR04306I';
             WVco#  = F3co#;
             WVfnd  = F3fnd;
             WVfyr  = F3fyr;
             WVemp# = F3emp#;
             WVdiv  = F3div;
             WVwav# = decWvr#;
             WVSWPC = ELwvPc;
             WVfChg = ELfChg;
             WVwMin = ELwMin;
             WVefDt = %dec(%char(%date(LDeeDt:*usa):*cymd0):7:0);
             WVEND# = LDend#;
             WVEMST = 'CA';
             WVENM  = F3form;
             // Map from 2 sets of 16 fields in WXFMP1 skip empty fields
             // Person or Organization
             If F3c401 <> *blanks;
                WVperOrg = %Trim(F3c401);
             Endif;
             If F3c402 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c402);
                Else;
                   WVperOrg = %Trim(F3c402);
                Endif;
             Endif;
             If F3c403 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c403);
                Else;
                   WVperOrg = %Trim(F3c403);
                Endif;
             Endif;
             If F3c404 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c404);
                Else;
                   WVperOrg = %Trim(F3c404);
                Endif;
             Endif;
             If F3c405 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c405);
                Else;
                   WVperOrg = %Trim(F3c405);
                Endif;
             Endif;
             If F3c406 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c406);
                Else;
                   WVperOrg = %Trim(F3c406);
                Endif;
             Endif;
             If F3c407 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c407);
                Else;
                   WVperOrg = %Trim(F3c407);
                Endif;
             Endif;
             If F3c408 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c408);
                Else;
                   WVperOrg = %Trim(F3c408);
                Endif;
             Endif;
             If F3c409 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c409);
                Else;
                   WVperOrg = %Trim(F3c409);
                Endif;
             Endif;
             If F3c410 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c410);
                Else;
                   WVperOrg = %Trim(F3c410);
                Endif;
             Endif;
             If F3c411 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c411);
                Else;
                   WVperOrg = %Trim(F3c411);
                Endif;
             Endif;
             If F3c412 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c412);
                Else;
                   WVperOrg = %Trim(F3c412);
                Endif;
             Endif;
             If F3c413 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c413);
                Else;
                   WVperOrg = %Trim(F3c413);
                Endif;
             Endif;
             If F3c414 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c414);
                Else;
                   WVperOrg = %Trim(F3c414);
                Endif;
             Endif;
             If F3c415 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c415);
                Else;
                   WVperOrg = %Trim(F3c415);
                Endif;
             Endif;
             If F3c416 <> *blanks;
                If WVperOrg <> *blanks;
                   WVperOrg = %Trim(WVperOrg) + ' ' + %Trim(F3c416);
                Else;
                   WVperOrg = %Trim(F3c416);
                Endif;
             Endif;

             // Blanket Waiver Project Description
             If F3c201 <> *blanks;
                WVblkPrjDs = %Trim(F3c201);
             Endif;
             If F3c202 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c202);
                Else;
                   WVblkPrjDs = %Trim(F3c202);
                Endif;
             Endif;
             If F3c203 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c203);
                Else;
                   WVblkPrjDs = %Trim(F3c203);
                Endif;
             Endif;
             If F3c204 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c204);
                Else;
                   WVblkPrjDs = %Trim(F3c204);
                Endif;
             Endif;
             If F3c205 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c205);
                Else;
                   WVblkPrjDs = %Trim(F3c205);
                Endif;
             Endif;
             If F3c206 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c206);
                Else;
                   WVblkPrjDs = %Trim(F3c206);
                Endif;
             Endif;
             If F3c207 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c207);
                Else;
                   WVblkPrjDs = %Trim(F3c207);
                Endif;
             Endif;
             If F3c208 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c208);
                Else;
                   WVblkPrjDs = %Trim(F3c208);
                Endif;
             Endif;
             If F3c209 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c209);
                Else;
                   WVblkPrjDs = %Trim(F3c209);
                Endif;
             Endif;
             If F3c210 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c210);
                Else;
                   WVblkPrjDs = %Trim(F3c210);
                Endif;
             Endif;
             If F3c211 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c211);
                Else;
                   WVblkPrjDs = %Trim(F3c211);
                Endif;
             Endif;
             If F3c212 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c212);
                Else;
                   WVblkPrjDs = %Trim(F3c212);
                Endif;
             Endif;
             If F3c213 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c213);
                Else;
                   WVblkPrjDs = %Trim(F3c213);
                Endif;
             Endif;
             If F3c214 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c214);
                Else;
                   WVblkPrjDs = %Trim(F3c214);
                Endif;
             Endif;
             If F3c215 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c215);
                Else;
                   WVblkPrjDs = %Trim(F3c215);
                Endif;
             Endif;
             If F3c216 <> *blanks;
                If WVblkPrjDs <> *blanks;
                   WVblkPrjDs = %Trim(WVblkPrjDs) + ' ' + %Trim(F3c216);
                Else;
                   WVblkPrjDs = %Trim(F3c216);
                Endif;
             Endif;

             WViUsr = F3iUsr;
             WViPgm = F3iPgm;
             WViDt  = F3iDt;
             WViTm  = F3time;
             WVuser = F3user;
             WVpgm  = F3pgm;
             WVdt   = F3dt;
             WVtime = F3time;
             Write pfrcdf;
             Delete pfm01;
             decWvr# = decWvr# + 1;
          Endif;
          Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pfm01;
       Enddo;
       //*** End Add *** 08/12/22 *********************************************

     C                   ENDSR                                                  PGMSTR
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C                   ENDSR
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C                   ENDSR
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL     DELETD        SCDATA
     C                   ENDSR
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
     C                   Z-ADD     SCDBRR        @@RRN
     C                   ENDSR
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================

      /Free
       Begsr Subset;
        meetss = *Off;
        Select;
        When @@cpgm = 'W0815R01';
           meetss = *On;
        When WVenm = ldWaiverFrm  // Called from Midterm Endorsements
         And xxWTyp <> *Blank
         And WVdiv = ELDiv;
           meetss = *On;
        When WVenm = ldWaiverFrm  // Called from W/W Employer Endorsements
         And (ldEnda =  *Blanks
          Or ldEnda = *Zeros);
           meetss = *On;
        When WVenm = ldWaiverFrm  // Called from W/W Employer Endorsements
         And xxWTyp = *Blank      //  for a midterm endorsement.
         And ldEnd# = WVend#;
           meetss = *On;
        Endsl;
       EndSr;
      /End-Free
** Column headings
Opt  Waiv#    Div     Name                            Project
Opt  Name                                 Div  Waiv#  Project
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
