     h*  (C) Copyright Progressive Data Solutions, Inc., 2005
     h**************************************************************************
     h/TITLE sc966r2- Creates document in IPDS
     h option(*nodebugio:*srcstmt)
     h**************************************************************************
     h*  DESCRIPTION:  This program receives the DARTS key and writes it as
     h*                the first record of the printer file, which is then
     h*                saved as a .txt file in the IFS
     h*
     h*  Called from:  SC966C2
     h*  Calls      :  None
     h*
     h**************************************************************************
     h*  Created on 07/18/2006 by c:002    Project 132IFV
     h**************************************************************************
     h* Revisions:
     h*
     h**************************************************************************
     fqsysprt   o    f  210        printer oflind(*inoa) usropn
      *
     d*=========================================================================
     d* Stand alone fields
     d*=========================================================================
     d ecKey           s             40
     d eRetrn          s              1
      *
      ** External data structure
     D psds          esds                  extname(szq1p)
      *=========================================================================
      * PLISTS
      *=========================================================================
     c     *entry        plist
     c                   parm                    ecKey
     c                   parm                    eRetrn
     c*=========================================================================
     c* Main Processing
     c*=========================================================================
     c*
     c* Initialization
     c*
     c                   open      qsysprt
     c                   except    head
     c                   close     qsysprt

     c                   eval      *inlr = *on
      *=========================================================================
      * Output files
      *=========================================================================
    o*
    o* Report Heading
    o*
     oqsysprt   e            head           1
     o                                           13 'Policy Nbr  :'
     o                       ecKey               54
     O          e            head        1  1
     o                                           13 'Rqsting User:'
     o                       q1user              54
