     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     H/TITLE SR48607I  - Additional Entry - Assumption of Liability
      *===================================================================*
      *                                                                   *
      *  CALLED FROM: SR48607                                             *
      *                                                                   *
      *===================================================================*
      *Revisions:                                                       *
      *                                                                   *
      * 02/21/17  C:180 Support 41395/SY371                               *
      *                 Scriptura interface. No change comments.          *
      *                                                                   *
      * 12/12/12  C:287 PROJECT MC2129                                    *
      *                 Created.                                          *
      *                                                                   *
      *===================================================================*

     FSR48607F  cf   e             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
      **Physical file definition (for maintaining record)
     Fwledp     uf a e             Disk
     F                                     rename( ped01 : dbrcdf )

      **Logical file definition (for maintaining record)
     Fwledl     if   e           k Disk    infds( edfds )

      **Group Master File
     FWMFNL     if   e           k Disk

      **Employer Master File
     FWMEML     if   e           k Disk

      **Carrier/State Reporting Master File
     FWMRPL2    if   e           k Disk

     D edfds           ds
     D  edrrn                397    400b 0

      *================================================================
      * /COPY required constants
      *================================================================
     D AddInF          c                   Const('Additional +
     D                                     Information for ')
     D comapi          c                   'SA005R'
     D #ofscr          c                   3

      *---------------------------------------------------------------------
      *Stand Alone Fields
      *---------------------------------------------------------------------
     D optnm           s             40
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
      *---------------------------------------------------------------------
     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
     D @@form          s                   like(fmform) inz('WC480607')

     D kkco#           s                   like( fmco# )
     D kkfnd           s                   like( fmfnd )
     D kkemp#          s                   like( fmemp# )
     D kkdiv           s                   like( fmdiv )
     D kkfyr           s              3  0
     D kkform          s                   like( fmform )
     D ##edn           s             80
     D cpol#           s               n                                        Is Policy# Carrier?

     C/COPY SCOPYMT01C

      *================================================================
      * PLIST definitions
      *================================================================

     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr

      **Company file/fund (group) retrieve program parameter list

     C     compl         plist
     C                   Parm                    ##co#
     C                   Parm                    ##conm

      *================================================================           `
      * KLIST definitions
      *================================================================

      **Employer Master file
     C     keyWMEML      Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkfnd
     C                   Kfld                    kkemp#
     C                   Kfld                    kkdiv

      **Additional Information file
     C     keyWLEDL      Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkfnd
     C                   Kfld                    kkemp#
     C                   Kfld                    kkdiv
     C                   Kfld                    kkfyr
     C                   Kfld                    kkform

      **Group Master file
     C     keyWMFNL      Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkfnd
      *
      /Free
        // ================================================================
        // *LIKE DEFN statements
        // ================================================================


        // ================================================================
        // DLTPRE -Pre-processing before delete occurs
        // ================================================================
           Begsr dltpre ;

           Endsr ;

        // ================================================================
        // DSPLYR -Processing after displaying information
        // ================================================================
           Begsr dsplyr ;
             //Reset error indicators
                %subarr( *in : 70 : 6 ) = *off ;
                *In90 = *Off ;
                cpol# = *Off ;
           Endsr ;

        // ================================================================
        // ERRCK1 -Error checking for srceen 1 (SCR01)
        // ================================================================
           Begsr errck1 ;

             //Reset Policy number error indicator
                %subarr( *in : 70 : 6 ) = *off ;

             //Validate Policy number; It cannot be blank.
             If scpol# = *blanks ;
               *In70 = *On ;
               *In90 = *On ;
             EndIf ;

             //Validate Policy period date
             If sceedt <> 0 ;
              srdti = sceedt ;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
      /Free
               If srErr <> '0' ;
                 *In71 = *On ;
                 *In90 = *On ;
               EndIf ;
             Else ;
               *In73 = *On ;
               *In90 = *On ;
             EndIf ;

             //Validate Policy Issued by; it cannot be blank.
             If scemnm = *blanks ;
               *In74 = *On ;
               *In90 = *On ;
             EndIf ;

             //Validate Name of Insured; it cannot be blank.
             If scinnm1 = *blanks ;
               *In75 = *On ;
               *In90 = *On ;
             EndIf ;

           Endsr ;

        // ================================================================
        // ERRCK2 -Error checking for srceen 2 (SCR02)
        // ===============================================================
           Begsr errck2 ;

           Endsr ;

        // ================================================================
        // ERRCK3 -Error checking for srceen 3 (SCR03)
        // ================================================================
           Begsr errck3 ;

           Endsr ;

        // ================================================================
        // MVDBSC -Move data base fields to screen fields
        // ================================================================
           Begsr mvdbsc ;

        //Default values if add mode
            If @@mode = 'A' ;
              Clear dbrcdf ;
              Chain(e) (kkco# : kkfnd : kkemp# : kkdiv : kkfyr) prp01 ;
                If %found( WMRPL2 ) ;
                   If RPCAP# <> ' ' ;  //If the carrier file contains Carrier Policy#
                     scpol# = RPCAP# ; //use that info; otherwise compose it from
                     cpol# = *On ;
                   Else ;              //Group#, Employer#, and Policy Year
                   scpol# = %char( RPFND ) +
                   %editc( RPEMP# : 'X' ) + %char( RPFYR ) ;
                   cpol# = *Off ;
                   EndIf ;
                Else ;
                   scpol# = %char( kkfnd ) +
                   %editc( kkemp# : 'X' ) + %char( kkfyr ) ;
                EndIf ;
                  If EMEFD1 <> 0 ;
                    srdti = EMEFD1 ;  //Use effective policy date
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
      /Free
                  Else ;
                   srdti = %dec( %date() ) ; //else use current date
      /End-Free
     C                   call      'SRCVTDAT'
     C                   parm                    srdti
     C                   parm      '*ISO'        srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      /Free
                  EndIf ;
              sceedt = %dec( srdto : 8 : 0) ;
              scemnm = FNNAME ;
              scinnm1 = EMNAME ;  //Use Employer Name as default
              scinnm2 = *blanks ;
            Else ;
              Chain keyWLEDL ped01 ;
               If %found( wledl ) ;
                 scpol# = FMCPL# ; //use that info; otherwise use Policy# field
                 srdti = FMRCDT ;
      /End-Free
     C                   call      'SRCVTDAT'    dtmdyy
      /Free
                 Eval sceedt = %dec( srdto : 8 : 0 ) ;
                 scemnm = FMEMNM ;
                 scinnm1 = %subst( FMINNM : 1 : 50 );  //Split Name of Insured in half for screen
                 scinnm2 = %subst( FMINNM : 51 : 50 );
                 sc1f01 = FM1F01 ;
                 sc1f02 = FM1F02 ;
                 sc1f03 = FM1F03 ;
                 sc1f04 = FM1F04 ;
                 sc1f05 = FM1F05 ;
                 sc1f06 = FM1F06 ;
                 sc1f07 = FM1F07 ;
                 sc1f08 = FM1F08 ;
                 sc1f09 = FM1F09 ;
                 sc1f10 = FM1F10 ;
                 sc1f11 = FM1F11 ;
                 sc1f12 = FM1F12 ;
                 sc1f13 = FM1F13 ;
               Endif ;
            Endif ;

        Endsr ;

        // ================================================================
        // MVSCDB -Move screen fields to data base fields
        // ================================================================
           Begsr mvscdb ;

            fmco# = kkco# ;
            fmfnd = kkfnd ;
            fmfyr = kkfyr ;
            fmemp# = kkemp# ;
            fmdiv = kkdiv ;
            fmform = kkform ;
            fmcpl# = scpol# ;
            fmemnm = scemnm ;
            srdti = sceedt ;
      /End-Free
     C                   call      'SRCVTDAT'    dtcymd
      /Free
            Eval FMRCDT = %dec( srdto : 7 : 0 ) ;
            FMINNM = scinnm1 + scinnm2 ;
            FM1F01 = sc1f01 ;
            FM1F02 = sc1f02 ;
            FM1F03 = sc1f03 ;
            FM1F04 = sc1f04 ;
            FM1F05 = sc1f05 ;
            FM1F06 = sc1f06 ;
            FM1F07 = sc1f07 ;
            FM1F08 = sc1f08 ;
            FM1F09 = sc1f09 ;
            FM1F10 = sc1f10 ;
            FM1F11 = sc1f11 ;
            FM1F12 = sc1f12 ;
            FM1F13 = sc1f13 ;

           Endsr ;

        // ================================================================
        // OTHERR -Processing for other modes
        // ================================================================
           Begsr  otherr ;

           Endsr ;

        // ================================================================
        // OTHFKS -Processing for other functions keys
        // ================================================================
           Begsr  othfks ;

           Endsr ;

        // ================================================================
        // PGMEND -End of program processing
        // ================================================================
           Begsr pgmend ;

             If *InKC = *On or *InKL ;
               @@call = 'F' ;
             Endif ;

           Endsr ;

        // ================================================================
        // PGMSTR -Start of program processing
        // ================================================================
           Begsr pgmstr ;

        //Clear screen formats

            Clear SCR01 ;
            Clear SCR02 ;
            Clear SCR03 ;

        // Get Mode
            kkco# = %dec( @@co#: 3 : 0 ) ;
            kkfnd = %dec( @@fnd : 3 : 0 ) ;
            kkemp# = %dec( @@emp# : 9 : 0 );
            kkdiv =  %dec( @@div : 5 : 0 ) ;
            kkfyr =  %dec( @@fyr : 3 : 0 ) ;
            kkform = @@form ;
    
           chain keyWLEDL ped01 ;
    
            If %found( wledl ) ;
              @@mode = 'C' ;
              @@rrn  = edrrn ;
            Else ;
              @@mode = 'A' ;
              @@rrn  = 0 ;
            Endif ;
    
        // Get Header
    
          optnm = AddInf + %trim( @@form ) ;
    
          If q1prms >= 1 and @@prds <> *blanks ;
            Eval ##data = @@prds ;
          Else ;
            Eval ##data = optnm ;
          Endif ;

      /End-Free
      *     Callp SRCENTER ( Data : 40 ) ;
     C                   Call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
      /Free
              eval scprds = ##data;
              eval scfnd = kkfnd;
              eval scfyr = kkfyr;

              Chain keyWMFNL pfn01 ;
                scfnnm = fnname ;

              Chain keyWMEML pem01 ;
      /End-Free
     C*         Callp WA005R ( ememp# : emdiv : emname : ##edn ) ;
      *
     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    ##edn
      *
      /Free
                eval scedn = ##edn ;

           Endsr ;

        // ================================================================
        //PREOTH -Pre-processing for other modes
        // ================================================================
           Begsr preoth ;

           Endsr ;

        // ================================================================
        //UPDATR -Update data base record after processing
        // ================================================================
           Begsr updatr ;
              Chain @@rrn dbrcdf ;
B01            If %Found ;
                 Exsr mvscdb ;
                 fmuser = q1user ;
                 fmpgm = q1pgm ;
                 Exsr Getdtm ;
                 Eval fmdt = %dec( srdto : 7 : 0 ) ;
                 Update dbrcdf ;
E01            Endif ;
             outflg  = *on ;
        Endsr ;

        // ================================================================
        // WRITER -Write data base record after processing
        // ================================================================
           Begsr Writer ;
             Clear dbrcdf ;
             Exsr mvscdb ;
             fmuser = q1user ;
             fmiusr = q1user ;
             fmpgm = q1pgm ;
             fmipgm = q1pgm ;
               Exsr Getdtm ;
             Eval fmdt = %dec( srdto : 7 : 0 ) ;
             Eval fmidt = %dec( srdto : 7 : 0 ) ;
             Write dbrcdf ;
             Feod wledp ;
             outflg = *on ;
           Endsr ;
      /End-Free
