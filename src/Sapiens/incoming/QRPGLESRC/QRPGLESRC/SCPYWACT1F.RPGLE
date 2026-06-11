    *===============================================================*
    *===============================================================*
    H/TITLE SCPYWACT1F - COPY 'F' Specs for Policy Action Pannel.
    *===============================================================*
    *  Created   : 03/07/12                                         *
    *  Programmer: C:190                                            *
    *  Project   : WC2126                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*
    *  File Usage:                                                  *
    *                                                               *
    *  WMFNL  (PFN00):  CO#/FND                                     *
    *  WMEML  (PEM00):  CO#/FND/EMP#/DIV                            *
    *  WDCXL1 (PCX01):  CO#/FND/EMP#/DIV                            *
    *  FWMAHP (PAH0P):  RRN                                         *
    *  FWMRPL (PRP00):  CO#/FND/EMP#/DIV/FYR                        *
    *===============================================================*
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwdcxl1    if   e           k disk    rename(pcx01:pcx01)
     fwmahp     if   e             disk    rename(pah01:pah0p)
     fwmrpl     if   e           k disk    rename(prp01:prp00)
