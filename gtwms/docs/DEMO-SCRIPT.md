# GT Warehouse Mobile — demo script

**30 minutes. 8 acts, ~22 minutes of driving, ~8 for questions.** Read this
along`GT-demo-barcode-cheatsheet.pdf` — the step numbers are the same in both.

Generate the sheet **on the day**:

```bash
node gtwms/tools/gen-demo-sheet.js      # re-reads TIGERPOC, writes the PDF
node gtwms/tools/verify-demo-sheet.js   # decodes every barcode independently
```

The sheet is built from live data. A receipt posted in rehearsal changes which
one it prints, so a stale sheet is the one thing that will embarrass you.

---

## Before you start

| | |
|---|---|
| Print the sheet at **100%** | "Fit to page" shrinks the modules and the camera starts missing them |
| Or show it on a **second screen** | Works fine — a laptop screen at full brightness scans better than paper |
| **Pull-to-refresh** the app first | Android caches Genie's `start.html` hard, and the viewport fix lives in it |
| Sign in and stop at the 5250 menu | Do not open option 4 until the audience is watching |
| Have the **spare receipt** ready (3.5) | If you rehearsed with 3.1 it is already posted |

**Two rehearsal facts.** Acts 3 → 4 are a chain: Receiving's POST is what creates
the Putaway work. And Act 6 needs a *deliberate* miscount, or there is no variance
and no photograph.

---

## The one-sentence framing

> "This is a stock system on the IBM i — RPG, DB2, commitment control, the same
> transactions you already run — with a phone as the terminal instead of a
> $2,000 gun. Nothing is re-platformed. The camera is the scanner, and the camera
> is also the evidence."

---

## ACT 0 · The way in (1 min)

**Do:** take option **4** on the sign-on menu.

**Say:** "This is a green-screen menu. Option 4 is a Rich Display program — same
job, same library, same library list."

**Point at:** the eight tiles and the badges. "Those numbers are DB2 counts read
at entry. 51 replenishment alerts, units sitting in staging. This screen already
knows what the shift looks like."

---

## ACT 1 · Scan Lab — one camera, every symbology (4 min)

**Do:** tile **Scan Lab** → open the camera → work down section 1 of the sheet.

**Say first:** "Before any application logic — can the phone actually read what
is on the boxes? A laser gun reads one family of codes. Here is what this camera
does."

**Scan in this order.** Do not scan all fifteen; pick the arc:

| Step | Card | The line |
|---|---|---|
| **1.1** | UPC-A | "Retail each. It came back as an item with a description." |
| **1.3** | ITF-14 | "Same product, but the **case**. It knows a case is 12." |
| **1.5** | SSCC-18 | "An 18-digit pallet licence plate. It resolved to a receipt." |
| **1.7** | **GS1-128, four AIs** | The showpiece — see below. |
| **1.9** | Code 39 badge | "That is a person, not a product. Same camera." |
| **1.11** | **QR** | "Same location as the Code 39 two cards up — 2D, identical answer." |
| **1.13/1.14** | PDF417 / Aztec | "A laser gun cannot read either of these." |
| **1.15** | Bad check digit | "And it refuses this one." |

**On 1.7, slow down.** "One scan. GTIN, lot number, expiry date and quantity —
four fields, one motion, no typing. That is the GS1 standard your vendors already
print. Most systems make somebody key the lot and the date."

**On 1.15:** "The check digit is wrong. It could have shrugged and looked it up
anyway — plenty do. It says no. That is the difference between reading a barcode
and trusting one."

**If a code will not read:** move the phone 15–20cm back, not closer. If it still
will not, say so and move on — "that is the honest answer for a 2D code on a
laser gun too."

---

## ACT 2 · Item Lookup — and the first photograph (3 min)

**Do:** back to menu → **Item Lookup** → scan **2.1** (UPC-A `061414102377`).

**Lands on:** *Ladies Fleece Hoodie – X-Large*, `APPL-000237`, image carousel,
on-hand by location.

**Say:** "Catalogue facts, the case pack, retail price, and every location holding
it. On a gun this is three screens."

### Then take the photograph

**Do:** tap the **camera button on the bottom-left of the carousel** → shoot →
**Use**.

**Say while it uploads:** "That photo just went into DB2 as a journaled BLOB —
same database as the stock balance, same backup, same audit. It is not in a
folder on somebody's phone. And it is in the carousel now, next to the catalogue
shot."

**Point at:** the new image appearing in the carousel immediately.

**Optional (2.2):** scan the ITF-14 case code — same item, resolved as a case.

---

## ACT 3 · Receiving — pallet to posted (5 min)

**Do:** menu → **Receiving** → scan **3.1**, the pallet SSCC
`306141410000000075`.

**Lands on:** `RC00000007`, **9 lines**, door **D007**, Northbridge Wholesale Ltd.

**Say:** "One scan of the pallet label and the receiver is on the right document.
No searching, no keying a PO."

**Do, in order:**

1. Scan **3.2** — an item on the receipt. "The line highlights and takes the
   expected quantity. That is scan-to-confirm: he never touches the screen."
2. Scan **3.3** — the second line, then **edit the quantity down** by hand and
   press **Save quantities**. "Short shipment. Two cases missing."
3. Point at the header: it has moved from *Not started* to **In progress**.
   "That happened inside the same commit as the line."
4. Tap **Post**.

**Say on the post:** "That is one unit of work. Stock into staging, the audit
rows, the pallet status, the receipt header — all of it commits together or none
of it does. If the phone dies mid-post, nothing is half-received."

**Do not skip the post — Act 4 depends on it.**

---

## ACT 4 · Putaway — the other half (3 min)

**Do:** menu → **Putaway**.

**Say:** "What you just received is now standing in staging. It cannot be picked
and it is in the way of the next truck. This is that queue."

**Do:** scan **4.1**, the staged pallet `306141410000000044` — its rows flash.

**Then 4.2:** scan the hero item's UPC to open the confirm screen.

**Before the screen settles, say the answer out loud:**

> "It is going to send this to **D05102** — a bulk bay. This SKU has no pick face
> of its own yet, and that bay is already holding 258 of them with room for 214
> more. So: consolidate, do not scatter."

*(Checked against `GTVPUTSUG` at sheet-generation time: `D05102` wins at rank 2 —
"a bulk bay this SKU already occupies" — because there is no rank-1 candidate,
i.e. no pick face assigned to this item. The rank-3 alternatives on screen are
empty pick faces. If you regenerate the sheet and the suggestion changes, the
card in section 4 will say so and this paragraph is what needs re-reading, not
the app.)*

**Then show the screen agreeing.** That is the moment: the audience sees the
system reasoned rather than guessed, because you said it first.

**Say:** "It suggests. He decides. The alternatives are right there, and he can
scan any location instead —" **do 4.3**, scan `D01012`, and the badge flips to
**SCANNED**. Then tap **Use suggestion** to go back.

**Do:** **Put away**. Point at the before/after: staging down, destination up.

---

## ACT 5 · Inventory & Movement — one box, three meanings (3 min)

**Do:** menu → **Inventory** → scan **5.1**, `D01012`.

**Say:** "Location, contents, capacity, and which lines are below minimum."

**Then the point of the screen:** "That same box takes a location, an item **or**
a pallet, and works out which it got. The operator never tells it."

**Do:** scan **5.3**, the item UPC — it goes to Item Lookup. "'Where is this?' and
'what is this?' are the same question, so it reuses the screen you already saw
rather than a near-duplicate."

**Then the alert path:** back, scan **5.2** (`B06101`) — the worst replenishment
alert. Tap through to the move, take the suggested quantity, book it.

**Say:** "Bulk to pick face, committed, audited. The alert list is the same 51 the
menu badge counted — one definition of 'needs replenishing', in a view, so the
menu and this screen cannot disagree."

---

## ACT 6 · Cycle Count — blind, reveal, evidence (4 min)

**Do:** menu → **Cycle Count** → scan **6.1**, `D01011` → count `CN00000001`,
26 lines.

**Say, and let it land:** "Look at this screen. There is **no expected quantity
anywhere on it.** That is not hidden with CSS — the number never leaves the
database. He cannot count to the answer because he does not have the answer."

**Do:** scan **6.2** (`061414100663`, *Liquid Laundry Detergent – 500 mL*) and
enter a count that is **clearly short** — the book figure is 833, so enter
something like **700**. Then count one more line and enter a number.

**Do:** tap **Review**.

**Say:** "Now — and only now — it shows him the book figure and the variance."

**Do:** pick reason **`CVAR` — Count variance**.

**Say:** "That reason code requires a photograph. Not because somebody coded an
`if` for it — because the reason-code table says `photo_required = Y`. Add a
reason tomorrow, tick the box, and the rule exists."

### The second photograph

**Do:** take the photo → **Use** → **Post**.

**Say:** "A supervisor reviewing that adjustment next week gets the count, the
variance, the reason, the operator, the timestamp — and the picture of the shelf."

---

## ACT 7 · Supervisor View (2 min)

**Do:** menu → **Supervisor**.

**Say:** "Everything you just did, aggregated. Those progress bars moved while we
were talking. Units in staging went down when you put that pallet away. That
count is now sitting in the variance queue."

**Do:** tap the `RC00000007` row.

**Say:** "And it is not a report — tapping a row opens the *actual screen*. A
dashboard nobody can act on is a poster."

Come back out.

---

## ACT 8 · Settings, and the close (1 min)

**Do:** menu → **Settings** → tap **Français**.

**Say:** "Whole application. One column in DB2, and every screen honours it —
including the reason codes and the vendor-facing text."

**Do:** tap **Large touch → On** — the controls grow on screen.

**Close on:**

> "Eight screens, RPG and DB2 on the IBM i, commitment control on every
> transaction, GS1 barcode handling, camera capture into the database, and
> bilingual — running on a phone your staff already own. Nothing here is a
> prototype pretending to be a system. It is your system, with a better terminal."

---

## If something goes wrong

| Problem | Do this |
|---|---|
| A barcode will not scan | Move **back** 15–20cm. Then use the keyed box — every screen has one, and say so: "and if the camera is having a bad day, he types it." |
| Screens look like a shrunken desktop | Pull-to-refresh. The viewport fix lives in Genie's `start.html`. |
| Images are missing | `node gtwms/tools/test-images.js` will say so in 20 seconds. Re-export and upload. |
| Receipt 3.1 already posted | Use the spare, card **3.5**. |
| Putaway queue empty | Act 3's post was skipped, or already put away. Post the spare receipt. |
| The count is already counted | Any of the other open counts works; scan its location instead. |

---

## What is deliberately not in the demo

Say these if asked — they are scope, not gaps:

- **Per-line damage photographs and reason codes in Receiving** (`rcvdetl`) and
  the **driver signature** (`rcvconf`). Designed, not built.
- **Large touch applies on the Settings screen only** so far.
- Picking, shipping and labour standards are out of scope for the POC.
