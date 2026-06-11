      * Copyright('(c) 2023 Sapiens. All Rights Reserved.')
      *===============================================================*
      /TITLE SCOPYEN01C - /COPY 'C' specs for Endorsement programs    *
      *===============================================================*
      * Notes:                                                        *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      * Revisions:
      *
      *  02/17/25 C:079 Support: 148/117703
      *                 For single print endorsements provide specific
      *                 group and employer so that correct sig is found
      *
      *  07/25/24 C:333 Project: WC2978
      *                 WCPOLS WC040314B, WC040366, WC040602B
      *                 - Hardcode XTtype = 'PEO Client Endorsement' for
      *                   Form WC040314B.
      *
      *  01/27/23 C:291 Project: WC2863
      *                 Add Endorsement State Approved/Excluded
      *                 subroutine to be used by all the National
      *                 Endorsements print program.
      *
      *  10/26/22 C:333 Project: WC2834
      *                 Endorsement Recognition of Cancellation
      *
      *  10/25/22 C:079 Project: WC2856
      *                 Clear Scriptura Type field for Alt Employer
      *
      *  01/31/22 C:079 Project: WC2853
      *                 Save effective date of state reporting division
      *
      *  01/29/21 C:180 Support: 93990
      *                 Monitor error message when running SendToScriptura.
      *
      *  01/20/21 C:136 Support: 93990
      *                 Pass correct type to Scriptura for the generic
      *                 endorsement programs when printing from W/W
      *                 Endorsements. Change in SCOPYEN01D as well.
      *
      * 12/28/20 C:180 Project: WC2786
      *                Add unexpired date logic to allow endorsements
      *                to attach to existing/active policies.
      *
      * 10/28/20 C:079 Project: WC2671
      *                Audit Request changes
      *
      * 01/31/19 C:136 Support: 77734
      *                Provide WXFMPMULTI for endorsements needing
      *                multiple entries on schedule.
      *
      * 12/13/19 C:291 Project: WC2693
      *                Add Date control logic for Endorsement State
      *                Includsion/Exclusion
      *
      * 04/19/19 C:300 Support: 77030 (LUBA)
      *                Resolve WC000106A form to iDARTS under division 0
      *
      * 01/23/19 C:136 Support: 73211
      *                Process multiple workfile records per
      *                endorsement. To implement, Exsr OpenWorkFile
      *                at beginning of OTHINF and Exsr CloseWorkFile
      *                at end. Replace WriteWorkFile with
      *                WriteWXWXFMP. Set WriteInCopyBook *Off in
      *                PGMSTR routine of the endorsement program.
      *
      * 11/02/18 C:65  Project: Phw#73970 (LUBA)                      *
      *                Data flag must be 'Y' for single endorsement   *
      *                print requests - to create SXDOP record.       *
      *                                                               *
      * 01/31/18 C:62  Project: 150WCO                                *
      *                Add optional form # override                   *
      *                                                               *
      * 01/09/18 C:62  Project: 133WQH                                *
      *                Add adoption date processing                   *
      *                                                               *
      * 11/08/17 C:65  Project: 150WCC                                *
      *                Populate rating state and generate from PDF    *
      *                values into work file.                         *
      *                                                               *
      * 10/06/17 C:136 Project: 150WAP                                *
      *                Add subroutine to check if endorsement passes  *
      *                the PEO relationship test before allowing to   *
      *                attach based on new setup flag.                *
      *                                                               *
      * 08/03/17 C:180 Support: On demand per David Miller.           *
      *                Change LDA access.                             *
      *                                                               *
      * 03/01/17 C:136 Project: WC2481                                *
      *                Add policy from and to dates to WXWXFMP        *
      *                                                               *
      * 12/28/16 C:136 Project: SY371                                 *
      *                Configure Scriptura for midterm endorsement    *
      *                processing                                     *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *                Use extended description field                 *
      *                                                               *
      * 07/17/16 C:302 Project: SY371                                 *
      *                Scriptura phase 2                              *
      *                Clean up Program/Copybook                      *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Subroutine Summary:                                           *
      *                                                               *
      * Internal Subroutines                                          *
      *  BASINF - Collect the basic info for WXFMP.                   *
      *  BULDIT - Gather information to merge to the form.            *
      *  CHEKIT - Determine whether or not to attach the form.        *
      *  GETAGT - Get the agent information.                          *
      *  GETCAR - Get the carrier information.                        *
      *  GETEMP - Get the employer information.                       *
      *  GETFND - Get the fund information.                           *
      *  GETFRM - Get the EnDorsement form information.               *
      *  INCREC - Write a merged forms data record.                   *
      *                                                               *
      * External Subroutines                                          *
      *  OTHINF - Get the form-specific information.                  *
      *  VALID  - Validate criteria for attaching form.               *
      ** Date check/conversion parameters
      *===============================================================*

     C     dtmdyy        PList
     C                   Parm                    srdti
     C                   Parm      '*CYMD'       srffmt
     C                   Parm      *blanks       srtfmt
     C                   Parm                    srdto
     C                   Parm                    srerr

     C     dtcymd        PList
     C                   Parm                    srdti
     C                   Parm      *blanks       srffmt
     C                   Parm      '*CYMD'       srtfmt
     C                   Parm                    srdto
     C                   Parm                    srerr
      ** Agent Assignment file
     C     aaky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxagt#
      ** Agent Master file
     C     agky          KList
     C                   KFld                    xxco#
     C                   KFld                    aafein
     C                   KFld                    aamod
      ** Agent History file
     C     ahky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxfyr
      ** Employer Detail file
     C     elky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxfyr
      *
     C     elky2         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxfyr
     C                   KFld                    xxemp#
      ** Employer Master file
     C     emky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
      ** Endorsement Form Table file
     C     enky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxform
      ** Group Master file
     C     fnky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
      ** Group Auxiliary file
     C     f2ky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
      ** Carrier file
     C     rpky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxfyr
      ** Rate Set Code file
     C     rsky          KList
     C                   KFld                    xxco#
     C                   KFld                    elrtcd
      ** Employer Attached EnDorsement Forms file
     C     muky          KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxfyr
     C                   KFld                    xxend#
     C     muky1         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxdiv
     C                   KFld                    xxfyr
     C                   KFld                    xxform
     c     esky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxform
     c*** Begin Add *** 01/09/18 *********************************
     c     esky2         klist
     c                   kfld                    xxco#
     c                   kfld                    xxform
     c                   kfld                    xxst
     c*** End   Add *** 01/09/18 *********************************

      *===============================================================*
      * Main
      *===============================================================*
      ** Additional input program parameters
     C     addpl         pList
     C                   parm      *blanks       qqprds
     C                   parm      qqmod2        qqcall
     C                   parm                    qqco#
     C                   parm                    qqfnd
     C                   parm                    qqemp#
     C                   parm                    qqdiv
     C                   parm                    qqfyr

     C     addpl1        pList
     C                   parm      *off          ##kc
     C                   parm      *off          ##kl
     C                   parm      qqmod2        ##call
     C                   parm      0             ##rrn
     C                   parm      qqrrn         ##rrn2
     C                   parm      1             ##view

     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3

     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
       ChkPgmInStack('W7231R':pgmInStack);
       //*** Beg Add *** 10/25/22 *********************************************
       scripturaType = *blanks;
       //*** End Add *** 10/25/22 *********************************************
       If #1Flag = '0' or (Q1pgm = 'SRALTEMP' and pgmInStack = *on);
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off;
          For idxGenPgms = 1 To #OfGenPgms;
             ChkPgmInStack(aryGenPgms(idxGenPgms):isGenPgmInStak);
             If isGenPgmInStak = *On;
                chrGenPgmType = aryGenType(idxGenPgms);
                Leave;
             Endif;
          Endfor;
       Endif;
     C                   Move      qqco#         xxco#
     C                   Move      qqfnd         xxfnd
     C                   Move      qqemp#        xxemp#
     C                   Move      qqdiv         xxdiv
     C                   Move      qqfyr         xxfyr

     C                   MoveL     qqForm        xxForm
     c                   Eval      formName = %subst(xxForm :1 :2) + ' ' +
     c                             %subst(xxForm :3 :2) + ' ' +
     c                             %subst(xxForm :5 :2) + ' ' +
     c                             %subst(xxForm :7 :2) + ' ' +
     c                             %subst(xxForm :9 :2)

     C                   select
     C                   when      qqMode = '1' or qqMode = '4'
     C                   eval      addMode = *on
     C                   when      qqMode = '2'
     C                   eval      prtMode = *on
     C                   when      qqMode = '3'
     C                   eval      infoMode = *on
     C                   endsl

     C                   Exsr      Scriptura

     c                   time                    dsdttm
     c                   Eval      srdti = dsdate
     c                   Call      'SRCVTDAT'    dtcymd
     c                   MoveL     srdto         sysdat            7 0

     C                   Exsr      pgmstr

     C                   Select

      ** Mode 1 - Determine whether to attach this form to the policy
     C                   When      qqmode = '1'
     C                   Exsr      chekit

      ** Mode 2 - Gather information to merge to the form
     C                   When      qqmode = '2'
     C                   Exsr      buldit

      ** Mode 3 - Call additional information entry program
     C                   When      qqmode = '3'
     C                   Move      xxco#         qqco#
     C                   Move      xxfnd         qqfnd
     C                   Move      xxemp#        qqemp#
     C                   Move      xxdiv         qqdiv
     C                   Move      xxfyr         qqfyr

     C                   If        UseDfltAddParms = *on
     C                   Call      addpgm        addpl
     C                   Else
     C                   Call      addpgm        addpl1
     C                   Endif

      ** Mode 4 - Gather info to merge for mid-term endors processing
     C                   When      qqmode = '4'

     C     *dtaara       Define    *lda          svlda
     C                   in        svlda
     C                   out       svlda

     C                   Exsr      chekit

     C                   If        crtfrm = *on
     C                   Exsr      buldit
     C                   Exsr      increc
     C                   EndIf

     C                   Endsl

     C                   Exsr      pgmend

     C                   Move      *on           *inlr
     C                   Return
      *===============================================================*
      * BASINF - Subroutine to load basic WXFMP fields
      *===============================================================*
     C     basinf        begsr

     C                   time                    dsdttm

     C                   Move      xxco#         fmco#
     C                   Move      xxfnd         fmfnd
     C                   Move      xxfyr         fmfyr
     C                   Move      xxemp#        fmemp#
     C                   Move      xxdiv         fmdiv
     C                   MoveL     xxform        fmform
     C                   Move      rsjrst        fmjrst
     C                   Move      engpdf        fmgpdf

     C                   Move      eneddt        dsdat7
     C                   MoveL     dsmm          fmeddt
     C                   Move      dsyy          fmeddt

     C                   If        ldenda <> *blanks
     C                             and ldenda <> *zeros
     C                   Move      ldeedt        fmeedt
     C                   Eval      fmend# = ldend#
     C                   Else
     C     muky1         Chain     pmu02                              60
     C                   Eval      srdti = mueedt
     C                   Call      'SRCVTDAT'    dtmdyy
     C                   MoveL     srdto         fmeedt

     C                   Eval      fmend# = *zero

     C                   EndIf

     C     qqfnd         cat(p)    qqemp#:0      fmpol#
     C                   cat       qqfyr:0       fmpol#

     C                   MoveL     emtpob        fmtobc
     C                   MoveL     emname        fmemnm
     C                   MoveL     emfdnm        fmfenm
     C                   eval      fminnm = emname + emnam2
     C                   eval      fmifnm = emfdnm + emfdn2
     C                   MoveL     xxfnst        fmfnst
     C                   MoveL     fnname        fmfnnm
     C                   Eval      svdiv = xxdiv
     C                   Eval      xxenmp = 0

      * Get Premium Amount

     c                   Call      'SRPAMT'
     c                   Parm                    xxco#
     c                   Parm                    xxfnd
     c                   Parm                    xxfyr
     c                   Parm                    xxemp#
     c                   Parm                    xxenmp
     c                   Parm      *blank        xxclr             1

     C                   Eval      xxdiv = svdiv
     C     elky          Chain     pel01                              20
     C                   Eval      fmprem = xxenmp
     C                   Move      xxagt#        fmagt#
     C                   MoveL     agname        fmagnm
     C                   MoveL     agpst         fmagst
     C                   MoveL     xxcpl#        fmcpl#
     C                   Move      xxcarr        fmcarr
     C                   MoveL     xxead1        fmead1
     C                   MoveL     xxead2        fmead2
     C                   MoveL     xxead3        fmead3
     C                   MoveL     xxemst        fmemst

     C                   Move      elandt        dsdat7
     C                   Move      dsdd          fmanda
     C                   Move      dsmm          fmanmo

     C                   MoveL     q1user        fmuser
     C                   MoveL     q1pgm         fmpgm

     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         fmdt

     C                   MoveL     dstime        fmtime
     C                   MoveL     'A'           fmstat
     C                   MoveL     q1user        fmiusr
     C                   MoveL     q1pgm         fmipgm
     C                   Move      fmdt          fmidt

      /Free
       Monitor;
          FMpfdt = %Dec(%Char(%Date(ELpfdt:*Cymd):*Usa0):8:0);
       On-Error;
          FMpfdt = 0;
       Endmon;
       Monitor;
          FMptdt = %Dec(%Char(%Date(ELptdt:*Cymd):*Usa0):8:0);
       On-Error;
          FMptdt = 0;
       Endmon;
      /End-Free

     C                   Endsr
      *===============================================================*
      * BULDIT - Subroutine to gather info to merge to the form
      *===============================================================*
     C     buldit        begsr

      ** Open the endorsement form workfile
     C                   open      wxfmp

      ** Get the basic information
     C                   Exsr      getfnd
     C                   Exsr      getemp
     C                   Exsr      getagt
     C                   Exsr      getfrm
     C                   Exsr      getcar

      ** Clear out the record
     C                   clear                   pfm01

      ** Load basic WXFMP fields
     C                   Exsr      basinf

      ** Load form specific fields
     C                   Exsr      othinf
      /Free
          //* Write the output record
          If WriteInCopybook;
             Exsr WriteWorkFile;
          Endif;
      /End-Free
      ** Close the endorsement form workfile
     C                   close     wxfmp

     C                   Endsr
      *===============================================================*
      * CHEKIT -Subroutine to determine if this form should be attached
      *===============================================================*
     C     chekit        begsr

     C                   Move      *off          crtfrm

      ** Get the employer and form information
     C                   Exsr      getemp
     C                   Exsr      getfrm

      ** Get the State Reporting division state
     C     rsky          Chain     prs01                              20
     C                   Move      rsjrst        svjrst

      ** Validate the attachment of the form
      ** For each division, determine if employer meets criteria.
     C     elky2         Chain     pel02                              20
     C                   Dow       *in20 = *off
     C     rsky          Chain     prs01                              20
      *
     C                   If        (enst = *blank) or (rsjrst = enst)
     C                   exsr      chkstat
     C                   Exsr      ChkPEO
     C                   Exsr      ChkUnExp
      //***** Beg Add *** 10/26/22 ***********************************
       Exsr ChkCXDt;
      //***** End Add *** 10/26/22 ***********************************
     C                   If        Attach = 'Y'
      ** Do not attach enDorsement if policy eff. date is less than the
      ** endorsement edition date.
     C                   ExSr      GetEdDt
      /Free
       If ((ELpFdt <> ELpTdt) and  (ELpFdt >= xxEdDt)) or
          ((ELpFdt <> ELpTdt) and
           (ENunExpDt >= ELpFdt and ENunExpDt < ELpTdt));
          Exsr Valid;
       Endif;

      /End-Free

     C                   If        crtfrm = *on
     C                   leave
     C                   EndIf

     C                   EndIf
     C                   EndIf

     C     elky2         ReadE     pel02                                  20
     C                   EndDo

     C                   If        crtfrm = *on

      ** Open the Employer Attached Endorsement Forms file
     C                   open      wmmul4
     C                   Eval      xxend# = *zeros
     C                   Eval      xxseq# = *zeros

      ** If processing for Mid-Year Endorsements, retrieve the previous
      ** endorsement and sequence numbers.
     C                   If        ldenda <> *zeros
     C                             and ldenda <> *blanks
     C                   Eval      xxend# = ldend#

     C     muky          SetGT     pmu01
     C     muky          ReadPE    pmu01                                  20

     C                   If        *in20 = *on
     C                   Eval      xxseq# = 1
     C                   Else
     C                   Eval      xxseq# = museq# + 1
     C                   EndIf

     C                   EndIf

      ** Clear out the record
     C                   clear                   pmu01

      ** Load the record
     C                   time                    dsdttm

     C                   Move      xxco#         muco#
     C                   Move      xxfnd         mufnd
     C                   Move      xxemp#        muemp#
     C                   Move      xxdiv         mudiv
     C                   Move      xxfyr         mufyr
     C                   Move      xxend#        muend#
     C                   Move      xxseq#        museq#
     C                   MoveL     xxform        muform
     C                   MoveL     enst          muenst
     C                   MoveL     endesc        mudesc
     C                   MoveL     ENdsc2        MUdsc2
     C                   Move      elpfdt        mueedt
     C                   MoveL     'I'           muexfl
     C                   MoveL     enmnau        mumnau
     C                   MoveL     enprnt        muprnt
     C                   MoveL     'A'           mustat
     C                   MoveL     q1user        muuser
     C                   MoveL     q1pgm         mupgm
     C                   MoveL     dstime        mutime

     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         mudt

     C                   MoveL     q1user        muiusr
     C                   MoveL     q1pgm         muipgm
     C                   Move      mudt          muidt

      ** Write the record.
     C                   write     pmu01

      ** Close the Employer Attached Endorsement Forms file.
     C                   close     wmmul4

     C                   EndIf

     C                   Endsr
      *===============================================================*
      * GETAGT - Subroutine to get agent information
      *===============================================================*
     C     getagt        begsr

     C                   open      wmahl
     C                   open      wmaal1
     C                   open      wmagl

     C                   Eval      xxagt# = *zero

     C     ahky          SetGT     pah01
     C     ahky          ReadPE    pah01                                  20

     C                   Dow       *in20 = *off
     C                             and ahstat = 'I'
     C     ahky          ReadPE    pah01                                  20
     C                   EndDo

     C                   If        *in20 = *off
     C                   MoveL     ahagt#        xxagt#
     C                   EndIf

     C                   If        *in20 = *off
     C     aaky          Chain     paa01                              20
     C                   EndIf
     C                   If        *in20 = *off
     C     agky          Chain     pag01                              20
     C                   EndIf

     C                   close     wmahl
     C                   close     wmaal1
     C                   close     wmagl

     C                   Endsr

      *===============================================================*
      * GETCAR - Subroutine to get carrier information
      *===============================================================*
     C     getcar        begsr

     C                   open      wmrpl

     C     rpky          Chain     prp01                              20

     C                   MoveL     rpcap#        xxcpl#

     C                   close     wmrpl

     C                   Endsr

      *===============================================================*
      * GetEdDt - Get the edition date
      *===============================================================*
     C     GetEdDt       begsr

     c                   Eval      xxSt = RSjrSt
     c     esKy2         Chain     pes01
     c                   If        Not %Found(wtesl)
     c                   Clear                   ESaDte
     c                   EndIf

     c                   Select
     c                   When      Ae_Flag = 'A' And ESaDte <> 0
     c                   Eval      xxEdDt = ESaDte
     c                   When      ENaDte <> 0
     c                   Eval      xxEdDt = ENaDte
     c                   Other
     c                   Eval      xxEdDt = ENedDt
     c                   EndSl

     c                   Endsr
      *===============================================================*
      * GETEMP - Subroutine to get employer information
      *===============================================================*
     C     getemp        begsr

     C                   clear                   xxpfdt

     C     emky          Chain     pem01                              20

     C                   If        *in20 = *off

      ** Build up the address lines using mailing address data . . .
     C                   If        emmpfl = 'M'
     C                   MoveL(p)  emmad1        xxead1
     C                   MoveL     emmst         xxemst
     C                   Move      emmzip        xxzip
     C     emmcty        cat(p)    ',':0         xxcsz
     C                   cat       emmst:1       xxcsz
     C                   cat       xxzip1:1      xxcsz

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         xxcsz
     C                   cat       xxzip2:0      xxcsz
     C                   EndIf

     C                   If        emmad2 <> *blank
     C                   MoveL(p)  emmad2        xxead2
     C                   MoveL(p)  xxcsz         xxead3
     C                   Else
     C                   MoveL(p)  xxcsz         xxead2
     C                   clear                   xxead3
     C                   EndIf

     C                   Else

      ** . . .or build up the address lines using physical address data
     C                   MoveL(p)  empad1        xxead1
     C                   MoveL     empst         xxemst
     C                   Move      empzip        xxzip
     C     empcty        cat(p)    ',':0         xxcsz
     C                   cat       empst:1       xxcsz
     C                   cat       xxzip1:1      xxcsz

     C                   If        xxzip2 <> *blank
     C                             and xxzip2 <> *zero
     C                   cat       '-':0         xxcsz
     C                   cat       xxzip2:0      xxcsz
     C                   EndIf

     C                   If        empad2 <> *blank
     C                   MoveL(p)  empad2        xxead2
     C                   MoveL(p)  xxcsz         xxead3
     C                   Else
     C                   MoveL(p)  xxcsz         xxead2
     C                   clear                   xxead3
     C                   EndIf

     C                   EndIf

     C                   EndIf

      ** Get the Employer Detail record
     C     elky          Chain     pel01                              20

     C                   If        *in20 = *off
     C                   Eval      srdti = elpfdt
     C                   Call      'SRCVTDAT'    dtmdyy
     C                   MoveL     srdto         xxpfdt
       svStRptEfDt = ELpFdt;
     C                   EndIf
     C     rsky          Chain     prs01                              20

     C                   Endsr
      *===============================================================*
      * GETFND - Subroutine to get Group information
      *===============================================================*
     C     getfnd        begsr

     C                   open      wmfnl
     C                   open      wdf2l

     C     fnky          Chain     pfn01                              20

     C                   If        fnpst <> *blank
     C                   MoveL     fnpst         xxfnst
     C                   Else
     C                   MoveL     fnmst         xxfnst
     C                   EndIf

     C     f2ky          Chain     pf201                              20

     C                   Move      f2car#        xxcarr

     C                   close     wmfnl
     C                   close     wdf2l

     C                   Endsr
      *===============================================================*
      * GETFRM - Subroutine to get endorsement form table information
      *===============================================================*
     C     getfrm        begsr

     C     enky          Chain     pen01                              20

     C                   Dow       *in20 = *off
     C                             and enstat = 'I'
     C     enky          ReadE     pen01                                  20
     C                   EndDo

     C                   Endsr
      *===============================================================*
      * INCREC - Subroutine to write a merged forms data record
      *===============================================================*
     C     increc        begsr

     C                   open      wxicp

     c                   movel(p)  xxform        xxdcid           12
     c                   movel(p)  xxform        xxdcidtmp        12

     c                   if        (%subst(xxdcidtmp:9:1) <> ' ')
     c                             and (%subst(xxdcidtmp:9:1) <> '.')
     c                   eval      xxdcid = %subst(xxdcidtmp:1:8) +
     c                                      '.' + %subst(xxdcidtmp:9:3)
     c                   else
     c                   movel(p)  xxform        xxdcid           12
     c                   endif

     c     '.inc('       cat(p)    xxdcid:0      icstmt
     C                   cat       ',':0         icstmt
     C                   cat       'AMTEMP':0    icstmt
     C                   cat       ',)':0        icstmt

     C                   write     pic01

     C                   close     wxicp

     C                   Endsr

       // *** Beg Add * 07/17/16
     C     chkstat       BegSr
      * Clear State Array
     c                   clear                   state_array
      * Chain endorsement states - approved table
     c     esky          chain     pes01                              60

     c                   if        *in60 = *off
      *Set flag indicating
     c                   eval      ae_flag = esapex

     c                   z-add     1             i                 3 0
     c     esky          chain     pes01                              60
     c                   Dow       *in60 = *off

      /Free
        If ESexdt = 0;
           ESexdt = *hival;
        Endif;
        If ESaDte <= ELpfDt and ELpfDt <= ESexDt;
      /End-Free
     c                   if        state_array(i) = ' '
     c                   eval      state_array(i) = esst
     c                   eval      i=i+1
     c                   endif
      /Free
        Endif;
      /End-Free
     c     esky          reade     pes01                                  60
     c                   Enddo

     c                   Eval      Attach = 'N'

      * Chain endorsement states - approved table

     c                   EVAL      I=1
     c     rsjrst        lookup    state_array(i)                         60

     c                   if        Ae_flag = 'A' and *in60 = *on
     c                   Eval      Attach = 'Y'
     c                   LeaveSr
     c                   endif

     c                   if        Ae_flag = 'E' and *in60 = *off
     c                   Eval      Attach = 'Y'
     c                   LeaveSr
     c                   endif


     c                   else

     c                   Eval      Attach = 'Y'

     c                   endif

     C                   Endsr
     C*** begin add ***  10/06/17  **********************************
      *===============================================================*
      * ChkPEO - Subroutine to check if policy meets PEO relationship.
      *===============================================================*
     C     ChkPEO        begsr
      /free
          If ENPEOF = ' ';
             Leavesr;
          Else;
             Exsr GetFnd;
             Select;
             When ENPEOF = '1';
                If not (ELEmp# = 1 and (FNPEOF = 'S' or FNPEOF = 'C'));
                   attach = 'N';
                Endif;
             When ENPEOF = '2';
                If not (ELEmp# = 1 and (FNPEOF = 'M' or FNPEOF = 'C'));
                   attach = 'N';
                Endif;
             When ENPEOF = '3';
                If not (ELEmp# = 1 and FNPEOF <> ' ');
                   attach = 'N';
                Endif;
             When ENPEOF = '4';
                If not (ELEmp# <> 1 and (FNPEOF = 'M' or FNPEOF = 'C'));
                   attach = 'N';
                Endif;
             When ENPEOF = '5';
                If not (FNPEOF = 'M' or FNPEOF = 'C');
                   attach = 'N';
                Endif;
             Endsl;

          Endif;

      /end-free
     C                   Endsr                                                  ChkPEO subr
     C*** end add *****  10/06/17  **********************************

      /Free
       Begsr Scriptura;
       // Determine if this module get spool file and/or XML output

          ChkPgmInStack('W7051C1':pgmInStack);
          If pgmInStack = *On;
             processType = 'Mid-Term Endorsement';
          Else;
             processType = 'Policy Page';
          Endif;

          isUseSpoolOut = UseSpoolOut(processType);
          isUseXML = UseScriptura(processType) Or
             Use3rdXml(processType);

          If isUseXML;
             isScriptUsed = 'Y';
          Else;
             isScriptUsed = 'N';
          Endif;

       Endsr;


       Begsr WriteWorkFile;

          Monitor;

             If isScriptUsed = 'N' or isUseSpoolOut = *On;
                write pfm01;
             EndIf;

             If isScriptUsed = 'Y';

                If processType = 'Mid-Term Endorsement';
                   PrintingPolicy = *on;
                Endif;

                Exsr GetXMLFileNm;

                Open WXWXFMP;
                FMxml = inXML;
                FMxmlT = XPxmlT;
                FMFORMF = formName;
                FMformO = ENformO;
                write FMRecord;
                Close WXWXFMP;

                If Not PrintingPolicy;
                   inXML@ = inXML + '$';
                   SendToScriptura(inXML@);
                EndIf;

             Endif;

          On-error;
          Endmon;

       Endsr;

       Begsr GetXMLFileNm;

          If PrintingPolicy;
             In svlda;
             outCall = 'TEMPLRQST';
             inXML = ldaXML;
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn = processType;
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLTEML1;
             If %found(XMLTEML1);
                XTdcid = %Trim(xxForm) + '.SCD';
                If ENGPDF = 'Y';
                   XTdcid = %Trim(xxForm) + '.PDF';
                   XTGPDF = 'Y';
                Endif;
                XPXMLT = XTXMLT;
                If ENPathC <> ' ';
                   XTPath = ENPathC;
                Endif;
                If RunningSRText;
                   XTtype = 'Text Only Endorsement';
                Else;
                   XTtype = %SubSt(xxForm:1:8);
                Endif;
                //*** Beg Add *** 10/25/22 *************************************
                If scripturaType <> *blanks;
                   XTtype = scripturaType;
                Endif;
                //*** End Add *** 10/25/22 *************************************
                //*** Beg Add ***** 07/25/24 ***********************************
                If XTtype = 'WC040314';
                   XTtype = 'PEO Client Endorsement';
                Endif;
                //*** End Add ***** 07/25/24 ***********************************
                Update XT100;
             EndIf;

          Else;
             outCall = 'XMLREQUEST';
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn = processType;
             outMode = ' ';
             //*** Beg Chg *** 02/17/25 ***************************************
             //***StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             //***OutPrefix:OutString:'000':'000');

             StLetter(OutCall:InXML:InReturn:outMode:%editc(ELrrn:'X'):
             OutPrefix:OutString:%editc(ELCo#:'X'):%editc(ELfnd:'X'));
             //*** End Chg *** 02/17/25 ***************************************

             Chain inXML XMLPREL1;
             If %found(XMLPREL1);
                XPdata = 'Y';
                XPCat = 'PS';
                XPCo# = ELCo#;
                XPFnd = ELFnd;
                XPEmp# = ELEmp#;
                XPDiv = XXDiv;
                XPFyr = ELFyr;
                XPdcid = %Trim(xxForm) + '.SCD';
                If ENGPDF = 'Y';
                   XPdcid = %Trim(xxForm) + '.PDF';
                   XPGPDF = 'Y';
                Endif;
                If ENPathC <> ' ';
                   XPPath = ENPathC;
                Endif;
                Select;
                When runningSRText;
                   XPtype = 'Text Only Endorsement';
                When isGenPgmInStak = *On;
                   XPtype = chrGenPgmType;
                //*** Beg Add *** 10/25/22 ************************************
                When scripturaType <> *blanks;
                   XPtype = scripturaType;
                //*** End Add *** 10/25/22 ************************************
                Other;
                   XPtype = %SubSt(xxForm:1:8);
                Endsl;
                Update XP100;
             Endif;
          EndIf;

       Endsr;

       Begsr OpenWorkFile;
          If processType = 'Mid-Term Endorsement';
             PrintingPolicy = *on;
          Endif;
          Exsr GetXMLFileNm;
          Open WXFMPMULTI;
       Endsr;

       Begsr CloseWorkFile;
          Close WXFMPMULTI;
          If Not PrintingPolicy;
             inXML@ = inXML + '$';
             Monitor;
                SendToScriptura(inXML@);
             On-error;
             Endmon;
          EndIf;
       Endsr;

       Begsr WriteMultiRcd;
          FMxml   = inXML;
          FMxmlT  = XPxmlT;
          FMa70a += 1; // Used as sequence field for Hibernate
          FMFORMF = formName;
          FMformO = ENformO;
          Write FMMulti;
       Endsr;

    //*=============================================================== *
    //* ChkUnExp- Subroutine to check for unexpired policy             *
    //*=============================================================== *
       Begsr ChkUnExp;
       If ENunExpDt > *Zero and Elptdt <= ENunExpDt;
          Attach = 'N';
       Endif;
       Endsr;

    //*** Beg Add ***** 10/26/22 ************************************* *
    //*=============================================================== *
    //* ChkCXDt - Subroutine to get Cancellation date and check for    *
    //*           divison for flat cancelled                           *
    //*=============================================================== *
       Begsr ChkCXDt;
        xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
        If xxCxDate <= ELpfdt and xxCxDate <> *zeros;
           Attach = 'N';
        Endif;
       Endsr;
    //*** End Add ***** 10/26/22 ************************************* *

      //*** Beg Add ***** 01/27/23 ************************************* *
      //*=============================================================== *
      //* ChkApprExclSt - Check Approved/Excluded State                  *
      //*=============================================================== *
       Begsr ChkApprExclSt;
        Chain (Elco#:ELrtcd) prs01;
        isApprExclSt = *Off;
        Chain (ELco#:qqForm) pes01;
        Select;
        When Not %Found(wtesl) ;
           isApprExclSt = *On;
        When %Found(wtesl);
           ae_flag = ESapex;
           stateFound = *Off;
           Chain (ELco#:qqForm:RSjrst) pes01;
           If %Found(wtesl);
              If ESexdt = 0;
                 ESexdt = *hival;
              Endif;
              If ESaDte <= ELpfDt and ELpfDt <= ESexDt;
                 stateFound = *On;
              Endif;
           Endif;
           Select;
           When Ae_flag = 'A' and stateFound = *On;
              isApprExclSt = *On;
           When Ae_flag = 'E' and stateFound = *Off;
              isApprExclSt = *On;
           Endsl;
        Endsl;
       Endsr;
    //*** End Add ***** 01/27/23 ************************************* *

      /End-free
