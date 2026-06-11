     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE F2386R - Work With OWCP/USH&L Physician Fee Schedule
     H*****************************************************************
     H* CREATED ON 06/21/12 by C:277 for Project 114MFI
     H*****************************************************************
     H* REVISIONS:
     H*
     H*  07/09/24  C:342  Project: FS688                                                   *
     H*                   2024 USH&L Fee Schedule (Recompile)                              *
     H*
     FF2386F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMHLP     IF   E             DISK
     F                                     RENAME(PHL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMHLL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PHL01:DBRCDF)
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D CVTDATE         pr                  extPgm('SRCVTDAT')
     D  indatein                      8  0
     D  infromfmt                     8
     D  intofmt                       8
     D  Outdate                      10
     D  outerr                        1
     D*
     D ACDPGMCal       pr                  extpgm('F2386R01')
     D  inkc                          1
     D  inkl                          1
     D  inMode                        1
     D  inRrn                         9  0
     D*
     D*
     DCvtToDate        pr              d
     D numDate                        8  0 Const
     D
     DDateTo7          PR             7  0
     D date                            d   Const
     D
     DdtCheck          PR             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)

     D  indatein       s              8  0
     D  infromfmt      s              8
     D  intofmt        s              8
     D  Outdate        s             10
     D  outerr         s              1
     D  inkc           s              1
     D  inkl           s              1
     D  inMode         s              1
     D  inRrn          s              9  0
     D  Sf1pag         s              5  0
     D  @@Call         s              1
     D  @@Rrn          s              9  0
     D  XXEFDT         S              8  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('OWCP/USH&L Physician Fee -
     D                                     Schedule')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2386R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     C/COPY SCOPYWW01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
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
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*  Converts the date from MDYY to CYMD.
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C***  KEY1          KLIST
     C***                KFLD                    ##CO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKMOD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    HLCODE        KKCODE
     C     *LIKE         DEFINE    HLMOD         KKMOD
     C     *LIKE         DEFINE    HLEFDT        KKEFDT
     C     *LIKE         DEFINE    HLEFDT        S1EFDT
      /Free
         //CHKSSM - Set indicator 11 for subset message

       BegSr Chkssm;

B01      If Ssefdt <> *zeros or Ssstat <> *blanks;
B01        *in11 = *on;
B01      Endif;
E01
       EndSr;
E01
         //ERRCK1 - Additional errors on subfile selection

       BegSr Errck1;
       Endsr;

         //ERRCK2 - Error checking for the subset screen

       BegSr Errck2;

         If *in90 = *off and ssefdt <> *zero;

           indatein = ssefdt;
           infromfmt = *Blanks;
           intofmt = *Blanks;
           Outdate = *Blanks;
           Outerr = *Blanks;

             callp  CVTDATE(indatein:infromfmt:intofmt:Outdate:
                    Outerr);

           If Outerr = '1';
             *in88 = *on;
             *in90 = *on;
             leavesr;
           Else;
             S1efdt = dateto7(cvttodate(ssEfdt));
           Endif;

         Endif;

         If ssstat <> ' ' and ssstat <> 'A'
              and ssstat <> 'I';
           *in89 = *on;
           *in90 = *on;
         Endif;

       EndSr;

        //ERRCKP - Error checking for the prompt screen

       BegSr Errckp;
       EndSr;

        //INZSST - Initialize the subset fields

       BegSr Inzsst;

         If @@Call = '2';
           Ssstat = 'A';
         Endif;

       Endsr;

        //KEYINZ - Initialize key values for program start

       BegSr Keyinz;

         Kkcode = *loval;
         Kkmod  = *loval;
         Kkefdt = *loval;

       EndSr;

        //KJDO - F10=SEARCH SUBSET SCREEN.

       BegSr Kjdo;
       EndSr;

        //KEYSET - Set key values when doing "position to"

       BegSr KeySet;

          If Scpst1 <> *blanks;
            Kkcode = Scpst1;
          Endif;

          If Scpstc <> *blanks;
            Kkmod  = Scpstc;
          Endif;

          If Scpstd <> *Zeros;
            indatein = Scpstd;
            infromfmt = *Blanks;
            intofmt = *Blanks;
            Outdate = *Blanks;
            Outerr = *Blanks;

              callp  CVTDATE(indatein:infromfmt:intofmt:Outdate:
                     Outerr);

            If Outerr = '1';
              leavesr;
            Else;
              kkefdt = %Int(Outdate);
            Endif;

          Endif;

       EndSr;

        //KEYSFL - Set key values from subfile records for paging

       BegSr Keysfl;

         If *in21 = *on;

           Kkcode = Sccode;
           Kkmod  = ScMod;
           Kkefdt = Scefdt;

         Endif;

       EndSr;

        //KVDO - F21=PRINT LIST

       BegSr Kvdo;
       EndSr;

        //MVDBSC - Move data base fields to screen fields

       BegSr Mvdbsc;

         Sccode = Hlcode;
         Scmod  = Hlmod ;
         Scefdt = Hlefdt;
         xxefdt = dtCheck(Hlefdt);

           scdata = (Hlcode + '    ' + Hlmod + '       ' +
                    %editw(xxefdt:'  /  /    ') + '  ' +
                    %subst(Hldesc :1 :39));
           scstat = Hlstat;

       EndSr;

        //OPT20 - SUBROUTINE FOR OPTION 20
        //OPTION 20=CHANGE STATUS

       BegSr Opt20;

         inMode = 'S';
         inRrn  = Scdbrr;

         callp ACDPGMCal(inkc:inkl:inMode:inRrn);

       Endsr;

        //OTHFKS - Process other function keys

       BegSr Othfks;

         If  *inkv = *on;
            Exsr kvdo;
         Endif;

       EndSr;

        //OTHOPT - Process other options

       BegSr Othopt;

         If scsel = '20';
           Exsr Opt20;
         Endif;

       EndSr;

        //PGMEND - End of program processing

       BegSr Pgmend;

         *inlr = *on;

       EndSr;

        //PGMSTR - Start of program processing

       BegSr Pgmstr;

         *in20 = *on;
         Sf1pag = 11;

       Select;
        //Inquary
       When @@Call = '1';
       *in09 = *on;

        //Search
       When @@Call = '2';
       *in30 = *on;
       Endsl;

       EndSr;

        //READ   - Read next record from file

       BegSr Read;

         If *in21 = *on;
           Read Dbrcdf;
             If Not %Eof(Fmhll1);
               *in95 = *off;
             Else;
               *in95 = *on;
             Endif;

         Endif;
       EndSr;

        //READP  - Read previous record from file

       BegSr Readp;

         If *in21 = *on;

           Readp Dbrcdf;
             If Not %Eof(Fmhll1);
               *in95 = *off;
             Else;
               *in95 = *on;
             Endif;

         Endif;

       EndSr;

        //SETGT  - Set greater than on file

       BegSr Setgt;

         If *in21 = *on;
           Setgt Key1 DbrcdF;

             If %Found(Fmhll1);
               *in96 = *off;
             Else;
               *in96 = *on;
             Endif;
         Endif;

       EndSr;

        //SETLL  - Set lower limits on file

       BegSr Setll;

         If *in21 = *on;
           Setll Key1 Dbrcdf;

             If %Found(Fmhll1);
               *in96 = *off;
             Else;
               *in96 = *on;
             Endif;
         Endif;

       EndSr;

        //SF1UPD - Update subfile record when item is deleted

       BegSr Sf1upd;
       EndSr;

        //SLCTRD - Move fields for selected record to parameters

       BegSr Slctrd;

         @@Rrn = Scdbrr;

       EndSr;

        //SUBSET - Determine if record should be put in subfile

       BegSr Subset ;

         If ssefdt <> *zeros and s1efdt <> Hlefdt;
           Meetss = *off;
         Endif;

         If ssstat <> *blanks and ssstat <> Hlstat;
           Meetss = *off;
         Endif;

       EndSr;
      /End-Free
** Column headings
Code   Modifier   Eff. Date   Description                             Status
Description                                     Zip  Proc    Eff. Date Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           NNN
3
