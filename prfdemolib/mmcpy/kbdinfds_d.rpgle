      // @MBRTEXT: Keyboard information data structure copy member
      ********************************************************************* ****
      * Application: General
      *     Purpose: Keyboard information data structure
      *
      * Log:
      * 08/01/01  Billy Andrews  Created
      *
      ********************************************************************* ****

     d KbdInfDS        ds
     d  KbdKey               369    369

      * Named hexadecimal constants for function keys
     d F01             c                   const(x'31')
     d F02             c                   const(x'32')
     d F03             c                   const(x'33')
     d F04             c                   const(x'34')
     d F05             c                   const(x'35')
     d F06             c                   const(x'36')
     d F07             c                   const(x'37')
     d F08             c                   const(x'38')
     d F09             c                   const(x'39')
     d F10             c                   const(x'3A')
     d F11             c                   const(x'3B')
     d F12             c                   const(x'3C')
     d F13             c                   const(x'B1')
     d F14             c                   const(x'B2')
     d F15             c                   const(x'B3')
     d F16             c                   const(x'B4')
     d F17             c                   const(x'B5')
     d F18             c                   const(x'B6')
     d F19             c                   const(x'B7')
     d F20             c                   const(x'B8')
     d F21             c                   const(x'B9')
     d F22             c                   const(x'BA')
     d F23             c                   const(x'BB')
     d F24             c                   const(x'BC')

     d Enter           c                   const(x'F1')
     d PageUp          c                   const(x'F4')
     d PageDn          c                   const(x'F5')
