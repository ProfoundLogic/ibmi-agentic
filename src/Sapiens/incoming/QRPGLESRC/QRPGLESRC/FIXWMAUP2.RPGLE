     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE fixwmaup2                                                                     *
     H**************************************************************************************
     H*  Description:                                                                      *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/19/00           Programmer C:87          Project: supp wa093 #22620   *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     F
     Fwmaup     uf   e             disk
     Fwmeml     if   e           k disk                                         emp master

     D                 DS
     D effdt                   1     35  0 Dim(5)
     D  emefd1                 1      7  0
     D  emefd2                 8     14  0
     D  emefd3                15     21  0
     D  emefd4                22     28  0
     D  emefd5                29     35  0
     D*
     D*
     D                 DS
     D cxdt                    1     35  0 Dim(5)
     D  emcad1                 1      7  0
     D  emcad2                 8     14  0
     D  emcad3                15     21  0
     D  emcad4                22     28  0
     D  emcad5                29     35  0
     D*
     D cxdate          s                   like(auptdt)
     D*
     D x               s              3  0
     D y               s              3  0

     D*  Date/time data structure
     D                 DS
     D dsdttm                        14  0
     D  dstime                        6  0 Overlay(dsdttm)
     D  dsdate                        8  0 Overlay(dsdttm:7)

     DPSDS           ESDS                  Extname(SZQ1P)

     C     dtcymd        plist
     C                   parm                    srdat_in          8 0
     C                   parm      *blanks       srfr_fmt          8
     C                   parm      '*CYMD'       srto_fmt          8
     C                   parm                    srdat_out        10
     C                   parm                    srerror           1


     C*  Key list
     C     emky          klist
     C                   kfld                    kkco#             3 0
     C                   kfld                    kkfnd             3 0
     C                   kfld                    kkemp#            9 0
     C                   kfld                    kkdiv             5 0

     C                   time                    dsdttm

     C*  Main loop
     C                   read      pau01                                  60
     C                   dow       *in60=*off
     C                   if         aucxfl='Y'

     C                   eval      cxdate = 0
     C
     C*    Get emp master record and check for cancel date
     C                   eval       kkco# = auco#
     C                   eval       kkfnd = aufnd
     C                   eval       kkemp# = auemp#
     C                   eval       kkdiv = audiv
     C     emky          chain     pem01                              61
     C                   if        *in61=*off

     C                   eval      y = 0
     C                   do        5             x
     C                   if        effdt(x) <> 0
     C                             and auptdt > effdt(x)
     C                   eval       y = x
     C                   endif
     C                   enddo
     C
     C                   if        y <> 0
     C                             and cxdt(y) < auptdt
     C                   eval       cxdate = cxdt(y)
     C                   endif
     C
     C                   endif                                                  *in61=*off

     C*    If cancel date exists, update auptdt with it
     C                   if        cxdate<>0
     C                   eval       auptdt = cxdate
     C                   eval       auuser = q1user
     C                   eval       aupgm = q1pgm
     C                   z-add     dsdate        srdat_in
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdat_out     audt
     C                   update    pau01
     C                   endif

     C                   endif                                                  aucxfl='Y'
     C                   read      pau01                                  60
     C                   enddo                                                  *in60=*off


     C                   eval      *inlr = *on
