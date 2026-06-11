     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Agency Contact
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTCON
     h*  Calls to:
     h*
     h*  Created on 10/12/2021 by C:300 for Project WC2395
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h* 05/26/25  C:333 Project: WC3009                                *
     h*           Clms, Policy, Agncy contacts: Maint Info             *
     h*           - Adding LastUsed and LastUsedBy                     *
     h*                                                                *
     h* 03/22/24 C:333 Project: WC2826                                 *
     h*                Claims Contacts.                                *
     h*                                                                *
     h**********************************************************************
      * Agent Contacts File
     fwma5p     if   e             disk    rename(pac01:pac0s)
     fwma5pcrt  if   e           k disk    rename(pac01:pac02)
     f                                     prefix(PR_)
      * Agent Contacts Types File
      *** Beg Chg ***** 03/22/24 ***********************************
     f***wtayl1    if   e           k disk
      * Contacts Types File
     fstcnl     if   e           k disk
      *** End Chg ***** 03/22/24 ***********************************
       //*** Beg Add *** 05/26/25 *******************************************
     fsmupl     if   e           k Disk    ReName(pup01:pup0l)
       //*** End Add *** 05/26/25 *******************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule
     d/copy sPrString

      * *ENTRY
     dwtAgtCon01       pr                  extpgm('WTAgtCon01')
     d                                9  0
     d                            32767
     d                                1

     dwtAgtCon01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

      *** Beg Del ***** 03/22/24 *********************************
     d*** strContType     s             50
      *** End Del ***** 03/22/24 *********************************
     d strAgtAdd1      s             40
     d strAgtAdd2      s             40
     d strAgtAdd3      s             40
     d strPhnNum       s             14
     d strCellNum      s             14
     d strFaxNum       s             14
      ******** Beg Add **** 05/26/25 *************************
     D strLstUpd       s             10
     D strLstUpdBy     s             50
      ******** End Add **** 05/26/25 *************************

      /Free

       Chain inRRN pac0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlLastName' :%trim(A5lsn));
       BuildJSON(outJSON :'OutDtlFirstName' :%trim(A5fsn));
       BuildJSON(outJSON :'OutDtlNameMI' :%trim(A5mid));
       BuildJSON(outJSON :'OutDtlCourtesyTitle' :%trim(A5titl));
       BuildJSON(outJSON :'OutDtlPosition' :%trim(A5posn));
       //*** Beg Del ***** 03/22/24 *********************************
       //***BuildJSON(outJSON :'OutDtlContactType' :%trim(strContType));
       //*** End Del ***** 03/22/24 *********************************
       BuildJSON(outJSON :'OutDtlAgtAdd1' :%trim(strAgtAdd1));
       BuildJSON(outJSON :'OutDtlAgtAdd2' :%trim(strAgtAdd2));
       BuildJSON(outJSON :'OutDtlAgtAdd3' :%trim(strAgtAdd3));
       BuildJSON(outJSON :'OutDtlAgentPhone' :%trim(strPhnNum));
       BuildJSON(outJSON :'OutDtlExtn' :%char(A5ext#));
       BuildJSON(outJSON :'OutDtlAgtCellPhone' :%trim(strCellNum));
       Select;
       When A5pphf = ' ';
          BuildJSON(outJSON :'OutDtlPrimaryPhn' :' ');
       When A5pphf = 'C';
          BuildJSON(outJSON :'OutDtlPrimaryPhn' :'Cell#');
       When A5pphf = 'P';
          BuildJSON(outJSON :'OutDtlPrimaryPhn' :'Phone#');
       Endsl;
       BuildJSON(outJSON :'OutDtlFaxNumber' :%trim(strFaxNum));
       BuildJSON(outJSON :'OutDtlEmaliAddr' :%trim(A5emal));
       //***** Beg Add **** 05/26/25 *************************
       BuildJSON(outJSON :'OutDtlALstUpd':strLstUpd);
       BuildJSON(outJSON :'OutDtlALstUpdBy':strLstUpdBy);
       //***** End Add **** 05/26/25 *************************

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        // Contact Type
        //*** Beg Del ***** 03/22/24 *********************************
        //***strContType = A5acty;
        //***IF A5acty <> *Blanks;
        //***   Chain (A5co#:A5acty) pay01;
        //***   IF %found(WTAYL1) and AYstat = 'A';
        //***      strContType = %trim(strContType) + ' - '  + AYdesc;
        //***   EndIf;
        //***EndIf;
        //*** End Del ***** 03/22/24 *********************************

        // Mailing Address
        strAgtAdd1 = %Trim(A5adr1);
        strAgtAdd2 = %Trim(A5adr2);
        strAgtAdd3 = %Trim(A5cty) + ', ' + A5st + ' ' +
         %Subst(%EditC(A5zip:'X'):1:5);
        If %Subst(%EditC(A5zip:'X'):6:4) <> *Blanks;
           strAgtAdd3 = %Trim(strAgtAdd3) + '-' +
            %Subst(%EditC(A5zip:'X'):6:4);
        EndIf;
        If strAgtAdd2 = *Blanks;
           strAgtAdd2 = strAgtAdd3;
           strAgtAdd3 = *Blanks;
        EndIf;
        If A5zip = *zeros;
           strAgtAdd1 = *Blanks;
           strAgtAdd2 = *Blanks;
           strAgtAdd3 = *Blanks;
        EndIf;

        // Phone Number
        If A5area <> *zeros;
           strPhnNum = '(' + %Subst(%EditC(A5area:'X'):1:3) + ')';
        EndIf;
        If A5phn# <> *zeros;
           strPhnNum = %trim(strPhnNum) + ' ' + %Subst(%EditC(A5phn#:'X'):1:3)+
           '-' + %Subst(%EditC(A5phn#:'X'):4:4);
        EndIf;

        // Cell Number
        If A5cara <> *zeros;
           strCellNum = '(' + %Subst(%EditC(A5cara:'X'):1:3) + ')';
        EndIf;
        If A5cphn <> *zeros;
           strCellNum= %trim(strCellNum) + ' ' + %Subst(%EditC(A5cphn:'X'):1:3)+
           '-' + %Subst(%EditC(A5cphn:'X'):4:4);
        EndIf;

        // Fax Number
        If A5fara <> *zeros;
           strFaxNum = '(' + %Subst(%EditC(A5fara:'X'):1:3) + ')';
        EndIf;
        If A5fphn <> *zeros;
           strFaxNum = %trim(strFaxNum) + ' ' + %Subst(%EditC(A5fphn:'X'):1:3)+
           '-' + %Subst(%EditC(A5fphn:'X'):4:4);
        EndIf;

        //*** Beg Add ***** 05/26/25 *************************
        strLstUpd = %Trim(%EditW(dtMDYY(A5dt):'  /  /    '));
        Chain A5user pup0l;
        If %found(SMUPL);
           strLstUpdBy = UPusnm;
        Endif;
        //*** End Add ***** 05/26/25 *************************


       endsr;
      /End-Free
