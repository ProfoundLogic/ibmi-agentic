     H* (C) Copyright Progressive Data Solutions, 1998
     H/TITLE W0320R01                                                                       *
     H**************************************************************************************
     H*  Description:  Connvert program to init catastrophe number in claim file. New      *
     H*                cat #'s are assigned during multiple claim # assignment process.    *
     H*                                                                                    *
     H*  Called from:                                                                      *
     H*  Calls to:                                                                         *
     H*                                                                                    *
     H*  Created: 08/11/98   Programmer C:87    Project:WC591                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FWMCML     UF   E           K DISK
     FWMCMLX    IF   E           K DISK    Rename(PCM01 : PCM02) Prefix(c2_)
     FWAMCL4    IF   E           K DISK
     FWAMCP     IF   E             DISK    Rename(PMC01 : PMC02) Prefix(m2_)

     D kkco#           S                   Like(MCCO#)
     D kkfnd           S                   Like(MCFND)
     D kkfyr           S                   Like(MCFYR)
     D kkemp#          S                   Like(MCEMP#)
     D kkdiv           S                   Like(MCDIV)
     D kkclm#          S                   Like(MCCLM#)
     D kkcat#          S                   Like(CMCATA)
     D kkcase          S                   Like(CMCASE)
     D*
     D found           S              1
     D cat#            S                   Like(CMCATA)

     DPSDS           ESDS                  Extname(SZQ1P)

     C*  Key list for multiple claim file
     C     mcky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkclm#
     C*
     C*  Key list for claim file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkcat#

     C*  Read through each record in multiple claim file
     C*
     C                   read      PMC02                                  50
     C                   dow       *in50 = *off
     C*
     C                   eval       kkco# = m2_MCCO#
     C                   eval       kkfnd = m2_MCFND
     C                   eval       kkfyr = m2_MCFYR
     C                   eval       kkemp# = m2_MCEMP#
     C                   eval       kkdiv = m2_MCDIV
     C                   eval       kkclm# = m2_MCCLM#
     C*
     C                   eval      found = '0'
     C                   eval      cat# = 0

     C*----------------------------------------------------------------------
     C*  Search for pre-existing catastrophe# for co#/fnd/fyr/emp#/div/clm#
     C*----------------------------------------------------------------------
     C     mcky          chain     PMC01                              60
     C                   dow       (*in60 = *off) and (found <> *on)
     C*
     C*    Check claim file to see if cat# is already assigned
     C                   move      MCCASE        kkcase
     C*
     C     cmky          chain     PCM01                              61
     C                   if        (*in61 = *off) and (CMCATA > 0)
     C                   eval       found = *on
     C                   eval       cat# = CMCATA
     C                   endif
     C*
     C                   if        found = *off
     C     mcky          reade     PMC01                                  60
     C                   endif
     C*
     C                   enddo                                                  *in60=*off

     C*----------------------------------------------------------------------
     C*  Assign catastrophe number to claim records
     C*----------------------------------------------------------------------
     C*    If cat# was not found, assign next cat#
     C                   if        found = *off
     C*
     C                   eval       kkcat# = *hival
     C     cmky1         setgt     PCM02
     C                   readp     PCM02                                  60
     C                   if        *in60 = *off
     C                   eval       cat# = c2_CMCATA + 1
     C                   else
     C                   eval       cat# = 1
     C                   endif
     C*
     C                   endif                                                  found=*off
     C*
     C*    Assign cat# to claim file corresponding to multiple claim file
     C     mcky          chain     PMC01                              60
     C                   dow       *in60 = *off
     C*
     C                   move      MCCASE        kkcase
     C*
     C     cmky          chain     PCM01                              61
     C                   if        *in61 = *off
     C                   eval       CMCATA = cat#
     C                   update    PCM01
     C                   endif
     C*
     C     mcky          reade     PMC01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C*    Read next record in multiple claim file
     C                   read      PMC02                                  50
     C                   enddo                                                  *in50=*off
     C*
     C                   eval       *inlr = *on
     C                   Return
