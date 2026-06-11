      /if defined(CONVERT)
     D ol              s             80    dim(14)
     D fl              s             80    dim(14)
     D vo              s              1    dim(99)
     D vf              s              1    dim(24)
    D**
     D ##ofd           ds          3500
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D dbfds3          ds
     D  dbrrn3               397    400b 0
     D dbfds4          ds
     D  dbrrn4               397    400b 0
     D dbfds5          ds
     D  dbrrn5               397    400b 0
     D dbfds6          ds
     D  dbrrn6               397    400b 0
     D dbfds7          ds
     D  dbrrn7               397    400b 0
     D dbfds8          ds
     D  dbrrn8               397    400b 0
     D dbfds9          ds
     D  dbrrn9               397    400b 0
     D psds          esds                  extname(szq1p)
     D fids01        e ds                  extname(szq2p)
    D*
     D rclcmd          c                   const('RCLRSC *CALLER')
     D pdsnam          c                   const('Stoneriver, Inc.')
     D wwcons          c                   const('Work with')
     D cdcons          c                   const('Confirm Delete of')
     D sscons          c                   const('Subset')
    D**

     *******************************************************************
     ******** Add by Profound Logic for Dynamic Function Keys **********
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     *******************************************************************
     *******************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##cmd           s             14
     D ##cmdl          s             15  5
     D ##data          s             40
     D ##leng          s              3  0
     D ##msgf          s             10
     D ##msgi          s              7
     D ##msgl          s             10
     D ##of            s            100
     D ##pgm           s             10
     D ##wwmo          s              1
     D @cont           s              1
     D @inc            s              7  0
     D @incc           s              7  0
     D @rcds           s              3  0
     D @readc          s              7  0
     D @readr          s              5  0
     D @reads          s              7  0
     D autorf          s              1
     D firsti          s              1
     D fl#1            s              3  0
     D fl#2            s              3  0
     D hd#             s              3  0
     D info#           s              1  0
     D l               s              1  0
     D lastrr          s              4  0
     D meetss          s              1
     D of#             s              3  0
     D ol#1            s              3  0
     D ol#2            s              3  0
     D outflg          s              1
     D scount          s              5  0
     D scuser          s             10
     D selmod          s              1
     D sfkey           s              5  0
     D sfrrn1          s              4  0
     D sl1flg          s              1
     D sngitm          s              1
     D sv95            s              1
     D test1l          s              1
     D test1r          s              1
     D vi              s              3  0
     D view#           s              1  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
      /Endif
