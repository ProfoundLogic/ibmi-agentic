     HCopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
      *
      *---------------------------------------------------------------------------*
      * Created    : 07/14/15                                                     *
      * Programmer : C:180                                                        *
      * Support    : 35571                                                        *
      *---------------------------------------------------------------------------*
      *                                                                           *
      * This pgm removes bogus records from the EDI log file WMLGP     ayment     *
      *                                                                           *
      *                                                                           *
      *---------------------------------------------------------------------------*
      * Revisions:                                                                *
      *                                                                            *
      *                                                                           *
     FWMLGP     UP   E             DISK
     FWMFNL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
      /Free
       If LGTSET = 'A49' and LGSTS = 'EC' and LGEXDT = *ZEROS;
          Chain (LGCO#:LGFND) PFN01;
          If %found;
             Chain (LGCO#:LGJST:LGTSET:FNEDIF) PIX01;
             If not %found or (%found and IXSTAT = 'I');
                Delete PLG01;
             Endif;
          Endif;
       Endif;
      /End-Free
