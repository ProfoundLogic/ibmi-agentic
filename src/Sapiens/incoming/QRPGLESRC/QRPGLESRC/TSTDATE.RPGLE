     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE FIXADDRESS                                                                     *
     H**************************************************************************************
     H*  Description:  Populate prov name/address in FAEAP                                 *
     H*                                                                                    *
     H*  Created: 05/02/03   Programmer C:79    Project:FL043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     Fqfefp     o    E             DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     C*
     C                   move      *date         sys_date
     C                   eval      efdt = sys_date
     C                   write     pef01
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
