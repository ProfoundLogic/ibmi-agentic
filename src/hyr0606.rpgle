**free

dcl-f hyrstubd workstn;

pgmname = 'HYR0606';
title   = 'Shipment Lot Inquiry';
line1   = 'Subfile-based inquiry over HYPSGLD (shipment lot';
line2   = 'detail) by turnaround / order / item.  Read-only';
line3   = 'view used by shipping personnel to verify what lots';
line4   = 'have been allocated to a shipment.';

exfmt stub;

*inlr = *on;
