     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE API - FA769R - Florida: Retrieve EOB Description         *
      //****************************************************************
      // Description:  Retrieves the EOB Description                   *
      //                                                               *
      //  Called From: F0340R - EOMB Print                             *
      //               W7002R - Check Print                            *
      //  Calls To:    None                                            *
      //                                                               *
      //****************************************************************
      // Created on 01/24/2013 by C:287   for  Project SY266_OVR       *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      // 10/28/24 C: 079 Tickett: 146/117939                           *
      //                 Monitor the OVRDBF command in case the state  *
      //                 table does not exist.                         *
      //                                                               *
      // 10/10/23 C: 079 Project: 99/114288                            *
      //                 Dont cut off words on EOB Code Description    *
      //                                                               *
      // 07/18/23 C: 079 Project: NY060                                *
      //                 Fix override logic and chain logic            *
      //                 OvrFile was not working consistently          *
      //                                                               *
      // 08/22/22 C: 333 Project: MC443                                *
      //                 Modernized EOB Print                          *
      //                                                               *
      // 09/23/13 C: 079 Support: MC147 83581                          *
      //                 Clear XNOTE prior to loading                  *
      //                                                               *
      //                                                               *
      //****************************************************************
     FFTABL     if   e           k Disk    UsrOpn
      //
     D psds          esds                  extname(szq1p)
      //
     D                 ds
     DTNote                    1    210
     D EBdesc                  1     70
     D EBdsc2                 71    140
     D EBdsc3                141    210
     D @@ast           s              2A
     D FTABLxx         s              7A
     D str             s              3s 0
     D end             s              3s 0
     D n1              s              3s 0
     D pos             s              3s 0
     D loop            s              1s 0
     D XXblk           s              1A
     D tmp1            s            140A
     D TMnot1          s            140A
     D/copy sprFiles
       Dcl-s @@eob Char(6);
       Dcl-s strCmd Char(56);
     D length          s             15  5
      //
      //================================================================
      // Parameter lists
      //================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@co#
     C                   PARM                    @@eob
     C                   PARM                    @@dsc1
     C                   PARM                    @@dsc2
     C                   PARM                    @@dsc3
     C                   PARM                    @@dsc4
     C                   PARM                    @@ast
      //================================================================
      // Key lists
      //================================================================
     C     EBKY          KLIST
     C                   KFLD                    KKco#
     C                   KFLD                    KKkey
      //================================================================
      // Like Defines
      //================================================================
     C     *like         define    EBco#         @@co#
     C*** Beg Del *** 07/18/23 *****************************************
     C     *like         define    EBdesc        @@dsc1
     C     *like         define    EBdsc2        @@dsc2
     C     *like         define    EBdsc3        @@dsc3
     C     *like         define    EBdsc4        @@dsc4
     C     *like         define    EBdesc        Xnote
     C     *like         define    EBco#         KKco#
     C     *like         define    EBkey         KKkey
     C                   move      @@co#         KKco#
     C                   movel     @@eob         KKkey
       // Only use the EOB code table if called with Adj. State
       Select;
       When @@Ast <> *blanks;
          //*** Beg Del *** 10/28/24 ***************************************
          //***If @@ast <> *blanks;
          //*** End Del *** 10/28/24 ***************************************
             strCmd = 'OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL' +
              %Trim(@@ast) + ')';
             length = 56;
             //*** Beg Add *** 10/28/24 ***************************************
             Monitor;
             //*** End Add *** 10/28/24 ***************************************
     C                   call      'QCMDEXC'                              90
     C                   parm                    strCmd
     C                   parm                    length
             //*** Beg Add *** 10/28/24 ***************************************
             On-Error;
             Endmon;
             //*** End Add *** 10/28/24 ***************************************
       //*** Beg Del *** 10/28/24 ***************************************
     C*** Else;
     C***    strCmd = 'OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABLFL)';
     C***    length = 56;
     C***                call      'QCMDEXC'                              90
     C***                parm                    strCmd
     C***                parm                    length
     C*** Endif;
       //*** End Del *** 10/28/24 ***************************************

          Monitor;
             Open FTABL;
          On-Error;
          Endmon;
          //*** Beg Add *** 10/28/24 ***************************************
          Monitor;
          //*** End Add *** 10/28/24 ***************************************
          Chain (kkCo#:kkKey) peb01;
          //*** Beg Add *** 10/28/24 ***************************************
          On-Error;
             EBdesc = @@dsc1;
             EBdsc2 = @@dsc2;
             EBdsc3 = @@dsc3;
             EBdsc4 = @@dsc4;
             Exsr SRwrap;
             Return;
          Endmon;
          //*** End Add *** 10/28/24 ***************************************
          If %Found(FTABL);
             @@dsc1 = *blanks;
             @@dsc2 = *blanks;
             @@dsc3 = *blanks;
             @@dsc4 = *blanks;
             exsr SRWRAP;
          EndIf;
          Monitor;
          DltOvr('FTABL');
          Close FTABL;
          On-Error;
          Endmon;
       When @@Ast = *blanks;
          EBdesc = @@dsc1;
          EBdsc2 = @@dsc2;
          EBdsc3 = @@dsc3;
          EBdsc4 = @@dsc4;
          Exsr SRwrap;
       Endsl;
       return;
       //---------------------------------------------------------------------
       //  SRWRAP : WORD WRAPPING LOGIC SO THAT WORDS DON'T CUT OFF
       //---------------------------------------------------------------------
       Begsr SRWRAP;
        //
        //* If there is no blank in the FIRST note , then we don't wrap
        //
     C     ' '           scan      TNOTE         pos                      98
        If *in98 <> *on
        Or pos > 70;
           If @@Ast <> *blanks;
              @@dsc1 = EBdesc;
              @@dsc2 = EBdsc2;
              @@dsc3 = EBdsc3;
              @@dsc4 = EBdsc4;
           Endif;
        else;
           //
           // MAKE SURE WORDS ARE NOT CUT OFF
           //
           str = 1;
           end = 0;
           n1  = 0;
           loop= 1;
           Dow loop <> 4;
           end = str + 70;
              If end >= 210;
                 XXblk = %Subst(EBdsc4:1:1);
                 end = 210;
                 Dow end <> 0;
                    If XXblk <> ' ';
                       end -= 1;
                    Else;
                       leave;
                    EndIf;
                 XXblk = %Subst(Tnote:end:1);
                 EndDo;
              Else;
                 DOW end <> 0;
                 XXblk = %Subst(Tnote:end:1);
                    If XXblk <> ' ';
                       end -= 1;
                    Else;
                       Leave;
                    EndIf;
                 EndDo;
              EndIf;
              n1 = end - str;
              Clear xnote;
              If n1 > 0;
                 Xnote = %Subst(Tnote:str:n1);
              Else;
                 Xnote = %Subst(Tnote:str:70);
              EndIf;
             //* REMOVE LEADING BLANKS FROM THE NOTE
     C     ' '           check     XNOTE         pos                      98
              If *in98 = *on
               AND pos > *zeros;
                 Clear tmp1;
                 tmp1 = Xnote;
                 Clear Xnote;
                 Xnote = %Subst(tmp1:pos:70);
              EndIf;
              Select;
              When loop =  1;
                 @@dsc1 = Xnote;
              When loop = 2;
                 @@dsc2 = Xnote;
              when loop = 3;
                 @@dsc3 = Xnote;
              EndSl;
              If end > 0;
                 str = end;
                 If n1 < 70;
                    str += 1;
                 EndIf;
              Else;
                 str += 70;
              EndIf;
              loop += 1;
           EndDo;
           //
           //** CATER TO LAST DESCRIPTION
           //
           pos = 0;
           TMnot1 = *blanks;
      //
     C     ' '           checkr    TNOTE:210     pos                      98
      //
           If *in98 = *on;
              pos = pos - str;
              pos += 1;
              If pos > *zeros;
                 TMnot1 = %Subst(Tnote:str:pos);
              EndIf;
           EndIf;
     C     ' '           checkr    TMNOT1:140    pos                      98
           If *in98 = *on;
     C                   cat       EBDSC4:0      TMNOT1
           Else;
     C                   movel     EBDSC4        TMNOT1
           EndIf;
           @@dsc4 = %Subst(TMnot1:1:70);
      //* REMOVE LEADING BLANKS FROM THE NOTE
     C     ' '           check     @@DSC4        pos                      98
           If *in98 = *on
            AND pos > *zeros;
              Clear tmp1;
              tmp1 = @@dsc4;
              Clear @@dsc4;
              @@dsc4 = %Subst(tmp1:pos:70);
           EndIf;
        EndIf;                                                                //
       Endsr;
      /END-FREE
