     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('STBNDDIR')
     H/define   profoundui
     H/TITLE STTEAMMBR- Work with team members
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Work With Team Members                  *
     H*                                                               *
     H* CALLED BY:                                                    *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 02/14/12  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/09/20   C:212 Support: 87446                               *
     H*                  delete records with status R to properly     *
     H*                  deallocate                                   *
     H*                                                               *
     H* 05/22/19   C:300 Support: 79584 (S1 Medical)                  *
     H*                  When setting up Teams the Sub-Teams assigned *
     H*                  to a team are not pulling the users from the *
     H*                  the sub-team.                                *
     H*                                                               *
     H* 04/23/2014 C:279 Support: OP099 84759                         *
     H*                  Corrected fix for support #84458.            *
     H*                                                               *
     H* 04/22/2014 C:279 Support: OP099 84759                         *
     H*                  Added edit for deleting Team Leader from     *
     H*                  drag drop option.                            *
     H*                                                               *
     H* 01/20/2014 c:62 Support: OP114 84458                          *
     H*                 Remove the user auth restriction so that all  *
     H*                 users appear on the selection window.         *
     H*                                                               *
     H*****************************************************************
     fstteammbr CF   E             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflTmStaff:RRNt)
     f                                     sfile(SflStaff:RRNs)
     f                                     sfile(SflTmLst:RRNl)
     fsmupl3    if   e           k disk    rename(pup01:pup03)
     fsmtel     if   e           k disk    rename(pte01:pte00)
     f                                     prefix(l_)
     fsmtep     if   e             disk    rename(pte01:pte0p)
     fsdtsl     uf a e           k disk    rename(pts01:pts00)
     D*
     D***ENTRY
     DMEMBERMAINT      PR                  EXTPGM('STTEAMMBR')
     D                                9
     D                                1
     D                               10
     DMEMBERMAINT      PI
     D inTeamRRN                      9
     D inMode                         1
     D inNextPgm                     10
     D*
     D/copy sprDate
     D/copy sprUsrPrms
     D/copy sprTeam
     D/copy sprAtrium
     D/copy sprString
      ******* beg add ****** 04/22/14 *************************************
     d/copy sprGenPop
      ******* end add ****** 04/22/14 *************************************
     D*
     DPassStaffFilter  pr              n
     D*
     DPassTeamFilter   pr              n
     D*
     DScanString       pr              n
     D inSearchTxt                   50    const
     D inSourceTxt                   50    const
     D*
     DWriteTeamMbr     pr
     D*
     DDeleteTeamMbr    pr
     D*
     DUpdateStatus     pr
     D inStatus                       1    const
      *
     DChkAuthority     pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inSTYP                         1    const
     D inSLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D*
     D psds          esds                  extname(szq1p)
     D*
     D flgLoad         s               n   inz
     D inUserType      s              1    inz
     D inUserLevel     s              3  0 inz
     D intCo#          s              3  0 inz
     D intTeamID       s              5  0 inz
     D intTeamRRN      s              9  0 inz
     D sourceUSR       s                   like(q1user) inz
     D sourceTtl       s             15    inz
     D sourceType      s              1    inz
     D strMgrID        s                   like(q1user)
     D strUser         s                   like(q1user)
     D strName         S             50
     D strPGM          S                   like(Q1USER)
     D strOutQ         s             10
     D strStaff        s              3
     D strRRN          s              9    inz
     D isError         s               n   inz
     D RRNs            s              5  0 inz
     D RRNt            s              5  0 inz
     D RRNl            s              5  0 inz
     D*
     D conBlack        c                   const('#000000')
     D conGrey         c                   const('#AAAAAA')
     D conBlue         c                   const('#0000CD')

      /Free
         Exsr Initialize;

       //***** Beg Add *** 04/09/20 ************************************
         // delete deallocated records with status R
         DelDeallocStsR();
       //***** End Add *** 04/09/20 ************************************

         DoU BtnBack = *On;
           If flgLoad = *On;
             Exsr LoadTeamMemberSFL;
           EndIf;
           If flgShwStaf = *On;
             Exsr LoadStaffsSubfile;
           EndIf;
           If flgShwTeam = *On;
             Exsr LoadTeamsSubfile;
           EndIf;

           sourceElem = '';
           sourceRec  = 0;
           targetElem = '';
           targetRec  = 0;

           ExFmt RcdMbrs;

           Select;
           When BtnBack = *On;
             Leave;
           When BtnRefresh = *On;
             flgLoad = *On;
             flgShwTeam = *Off;
             flgShwStaf = *Off;
           When BtnAddStaf = *On;
             flgShwStaf = *On;
             flgShwTeam = *Off;
             flgLoad = *Off;
           When BtnAddTeam = *On;
             flgShwStaf = *Off;
             flgShwTeam = *On;
             flgLoad = *Off;
           Other;
             ExSr CheckSFLOptions;
             ExSr HandleDragDropAction;
             If Not isError;
               flgLoad = *On;
             Else;
               flgLoad = *Off;
             Endif;
           Endsl;

         EndDo;

       //***** Beg Add *** 04/09/20 ************************************
         // delete deallocated records with status R
         DelDeallocStsR();
       //***** End Add *** 04/09/20 ************************************

         *InLr = *On;
         return;

       //*******************************************************************************************
       //  Initialize  Subroutine
       //*******************************************************************************************
       BegSr Initialize;

         strUser = q1user;
         strPGM = q1pgm;

         intCo# = GetUserInfo(strUser:strName:strOutQ:strStaff:
                   inUserType:inUserLevel);

         intTeamRRN = %int(inTeamRRN);
         chain intTeamRRN pte0p;
         intTeamID = TEID;
         SCTitle = %Trim(TEname) + ' - ' + %Trim(TEdesc);
         strMgrID  = TEMGR;

         flgShwStaf = *Off;
         flgShwTeam = *Off;
         flgLoad = *On;

         //set values for Team drag panel position
         scPnlTop = 80;
         scLabelTop = scPnlTop + 35;
         scFltrTop = scPnlTop + 60;
         scSubTop = scPnlTop + 80;
         scSFLTop = scPnlTop +105;

       EndSr;
       //*******************************************************************************************
       //  The CheckSFLOptions Subroutine
       //*******************************************************************************************
       BegSr CheckSFLOptions;

       ReadC sflTmStaff;
       If not %EOF;
         Select;
         When LnkStRmv= *On;
           If scStaff2 <> ' ';
             UpdateStatus('R');
             LnkStRmv = *Off;
           EndIf;
         When LnkReasign = *On;
           If scStaff2 <> ' ';
             UpdateStatus('A');
             LnkReasign = *Off;
           EndIf;
         EndSl;
         Update sflTmStaff;
       EndIf;

       EndSr;
       //*******************************************************************************************
       //  The LoadTeamMemberSFL    Subroutine
       //*******************************************************************************************
       BegSr LoadTeamMemberSFL;

         clrTmStaff= *On;
         Write RcdMbrs;
         clrTmStaff= *Off;
         RRNt = 0;

         SetLL intTeamId pts00;
         ReadE(N) intTeamId pts00;
         Dow NOT %EOF;
           RRNt +=1;
           scStaff2 = TSUsr;
           scType = TSType;

           If sctype = *blanks;
             scStaffNm2 = GetUserName(scStaff2);
             scRole = TSRole;
             flgSubTeam = *Off;
           Else;
             scStaffNm2 = GetTeamName(%Int(TSUsr));
             scRole = 'Sub-Team';
             flgSubTeam = *On;
             strRRN = %Editc(GetTeamRRN(%Int(TSUsr)):'X');
             scSTeamURL = CreateURL('STTEAMMST':scStaffNm2:strRRN);
           EndIf;

           erReasign = *Off;
           flgActive = *Off;
           flgReasign = *Off;
           flgBold = *Off;
           Select;
           When TSusr = strMgrID;
             // no links
             flgBold = *On;
             scLinColor = conBlack;
           When TSstat = 'A';
             flgActive = *On;
             scLinColor = conBlack;
             If TStype = 'T';
               scLinColor = conBlue;
             EndIf;
           When TSstat = 'R';
             scLinColor = conGrey;
             flgReasign = *On;
           EndSl;

           Write sflTmStaff;

           ReadE(N) intTeamId pts00;
         EndDo;

       EndSr;
       //*******************************************************************************************
       //  The LoadStaffsSubfile Subroutine
       //*******************************************************************************************
       BegSr LoadStaffsSubfile;

         clrStaff = *On;
         Write RcdMbrs;
         clrStaff = *Off;
         RRNs = 0;

         Setll *loval pup03;
         Read(N) pup03;
         Dow Not %EOF;
           //***** Beg Chg ****** 04/23/14 ***********************************
           //*** Begin Del *** 01/20/2014 ************************************
           // If PassStaffFilter();
           //*** End   Del *** 01/20/2014 ************************************
           If PassStaffFilter();
           //***** End Chg ****** 04/23/14 ***********************************
             RRNs +=1;
             scStaff1 = UPusr;
             scStaffNm = UPusnm;
             scStaffCd = UPstaf;
             scStaffTtl = UPtitl;
             Write SflStaff;
           //***** Beg Chg ****** 04/23/14 ***********************************
           //*** Begin Del *** 01/20/2014 ************************************
           // Endif;
           //*** End   Del *** 01/20/2014 ************************************
           Endif;
           //***** End Chg ****** 04/23/14 ***********************************
           Read(N) pup03;
         Enddo;

       EndSr;
       //*******************************************************************************************
       //  The LoadTeamsSubfile Subroutine
       //*******************************************************************************************
       BegSr LoadTeamsSubfile;

         clrTmLst = *On;
         Write RcdMbrs;
         clrTmLst = *Off;
         RRNl = 0;

         Setll *loval pte00;
         Read pte00;
         Dow Not %EOF;
           If PassTeamFilter();
             RRNl +=1;
             scTmID = l_TEid;
             scTmName = l_TEname;
             scTmDesc = l_TEdesc;
             scTmLdr = GetUserName(l_TEmgr);
             Write SflTmLst;
           Endif;
           Read(N) pte00;
         Enddo;

       EndSr;
       //*******************************************************************************************
       //  The HandleDragDropAction Subroutine
       //*******************************************************************************************
       BegSr HandleDragDropAction;

         Exsr GetSourceRow;

         Select;
         When (sourceElem = 'SflStaff' Or sourceElem = 'SflTmLst')
          And targetElem = 'SflTmStaff';
           //--------------------------------------------
           // Record Dragged to team Subfile
           //--------------------------------------------
           WriteTeamMbr();
         When targetElem = 'undefined' or targetElem = '';
           // If something is dragged onto an invalid element - do nothing
         When sourceElem = 'SflTmStaff' And
          (targetElem = 'SflStaff' Or targetElem = 'SflTmLst');
           //Team member/Sub-Team Record was Dragged
           //****** beg add ******* 04/22/14 *********************************
           If strMgrID = scStaff2;
              PopOneBtn('Team Leader can only be changed via Edit link on +
               Team Master Maintenance screen.':'Information');
           Else;
           //****** end add ******* 04/22/14 *********************************
           DeleteTeamMbr();
           //****** beg add ******* 04/22/14 *********************************
           Endif;
           //****** end add ******* 04/22/14 *********************************
         EndSL;
       EndSr;

       //*******************************************************************************************
       //  The GetSourceRow Subroutine  - determines the sequence number (sourceSEQ) of
       //                                 the subfile record that was dragged somewhere
       //*******************************************************************************************
       BegSr GetSourceRow;
         If sourceRec = 0;
           LeaveSr;
         EndIf;

         Select;
         When sourceElem = 'SflStaff';
           scStaffRRN = sourceRec;
           Chain scStaffRRN sflStaff;
           If %Found;
            sourceUsr = scStaff1;
            sourceTtl = scStaffTtl;
            sourceType = *blanks;
           EndIf;
         When sourceElem = 'SflTmLst';
           scTeamRRN = sourceRec;
           Chain scTeamRRN SflTmLst;
           If %Found;
            sourceUSR = %Editc(scTmID:'X');
            sourceTtl = 'Sub-Team';
            sourceType = 'T';
           EndIf;
         When sourceElem = 'SflTmStaff';
           scTmStRRN = sourceRec;
           Chain scTmStRRN SflTmStaff;
           If %Found;
            sourceUSR = scStaff2;
            sourceTtl = scRole;
            sourceType = scType;
           EndIf;
         Other;
           LeaveSr;
         EndSL;
       EndSr;
      /End-Free
       //*******************************************************************************************
       // PassStaffFilter
       //*******************************************************************************************
     PPassStaffFilter  b
     DPassStaffFilter  pi              n
     D*
     D strUserLvl      S              3
     D strStaffLvl     S              3
     D isRestricted    S               n
      /Free
        strUserLvl = %editc(inUserLevel:'X');
        strStaffLvl = %editc(UPlvl:'X');
        ChkAuthority(inUserType:strUserLvl:UPtype:strStaffLvl:'1'
          :isRestricted);

        //User inactive Or No Authority Or Already on this Team Or This user on Staff's team
        //****** beg chg ****** 04/23/14 ***********************************
        // If UPstat = 'I' OR isRestricted OR OnTeam(intTeamId:UPusr:*On)
        If UPstat = 'I' OR OnTeam(intTeamId:UPusr:*On)
        //****** end chg ****** 04/23/14 ***********************************
         Or OnUserTeams(UPusr:strMgrId);
          Return *off;
        EndIf;

        If Not ScanString(scNmFilter:UPusnm);
          Return *Off;
        EndIf;

        If Not ScanString(scTtlFltr:UPtitl);
          Return *Off;
        EndIf;

        Return *On;

      /End-Free
     PPassStaffFilter  e
       //*******************************************************************************************
       // PassTeamFilter
       //*******************************************************************************************
     PPassTeamFilter   b
     DPassTeamFilter   pi              n
     D strFilterTxt    S             50
     D strSearchTxt    S             50
      /Free
        If l_TEid = intTeamId;
          Return *Off;
        EndIf;

        //Check if team is already on User's team
        If OnTeam(intTeamId:%Editc(l_TEid:'X'):*On);
          Return *Off;
        EndIf;

        //***** Beg Del ****** 05/22/19 ***********************************
        //Check if User is on this team or any of its subteam
        //*****  If OnUserTeams(l_TEmgr:strMgrId);
        //*****    Return *Off;
        //*****  Endif;
        //***** End Del ****** 05/22/19 ***********************************

        If Not ScanString(scTmNmFtr:l_TEname);
          Return *Off;
        EndIf;

        If Not ScanString(scTmDscFtr:l_TEdesc);
          Return *Off;
        EndIf;

        Return *On;
      /End-Free
     PPassTeamFilter   e
       //*******************************************************************************************
       // UpdateStatus  - Update staff status
       //*******************************************************************************************
     PUpdateStatus     b
     DUpdateStatus     pi
     D inStatus                       1    const
      /Free
        erReasign = *Off;
        isError  = *Off;
        Chain(n) (intTeamID : scStaff2) pts00;
        If %Found(sdtsl);
          //Check for cyclic reference on Allocation
          If inStatus = 'A' And OnUserTeams(TSusr:strMgrId);
            erReasign = *On;
            isError = *On;
            mgReasign = 'Allocation cannot be processed,' +
             ' This will create cyclic reference.';
          else;
            Chain (intTeamID : scStaff2) pts00;
            TSstat = inStatus;
            TSuser = strUser;
            TSpgm = strPGM;
            TStime = TimeTo6(SystemTime);
            TSdt = DateTo7(SystemDate);
            Update pts00;
          Endif;
        EndIf;
      /End-Free
     PUpdateStatus     e
       //***** Beg Add *** 04/09/20 ************************************
       //*******************************************************************************************
       // DelDeallocStsR - Delete from SDTSP records with status R
       // These would be deallocated records that have R as status
       //*******************************************************************************************
     PDelDeallocStsR   b
     DDelDeallocStsR   pi
      /Free
        //delete all records with status R
        Setll *loval pts00;
        Read pts00;
        DoW Not %eof(sdtsl);
           If TSstat = 'R';
              delete pts00;
           EndIf;
           Read pts00;
        EndDo;
      /End-Free
     PDelDeallocStsR   e
       //***** End Add *** 04/09/20 ************************************
       //*******************************************************************************************
       // WriteTeamMbr- Write Team member record
       //*******************************************************************************************
     PWriteTeamMbr     b
     DWriteTeamMbr     pi
      /Free
       TSco# = intCo#;
       TSusr = sourceUSR;
       TStid = intTeamId;
       TSrole = sourceTtl;
       TStype = sourceType;
       TSstat = 'A';

       TSuser = strUser;
       TSpgm = strPGM;
       TStime = TimeTo6(SystemTime);
       TSdt = DateTo7(SystemDate);

       TSiusr = TSuser;
       TSipgm = TSpgm;
       TSitm = TStime;
       TSidt = TSdt;

       Write pts00;
      /End-Free
     PWriteTeamMbr     e
       //*******************************************************************************************
       // DeleteTeamMbr- Delete Team member record
       //*******************************************************************************************
     PDeleteTeamMbr    b
     DDeleteTeamMbr    pi
      /Free
       Delete (intTeamId:sourceUSR) pts00;
      /End-Free
     PDeleteTeamMbr    e
       //*******************************************************************************************
       // ScanString - Search text in source string
       //*******************************************************************************************
     PScanString       b
     DScanString       pi              n
     D inSearchTxt                   50    const
     D inSourceTxt                   50    const
     D strSearchTxt    S             50
     D strSourceTxt    S             50
      /Free
        strSearchTxt = inSearchTxt;
        strSourceTxt = inSourceTxt;

        strSearchTxt = Upper(strSearchTxt);
        strSourceTxt = Upper(strSourceTxt);
        If strSearchTxt <> *blanks And
         %Scan(%Trim(strSearchTxt):strSourceTxt) = 0;
          Return *off;
        EndIf;
        Return *On;
      /End-Free
     PScanString       e
