================================================================================
HOW DISTRICTS ARE IDENTIFIED FROM MCL JOB ADS
================================================================================
Code:    vn_district_match.R (matcher) + Cleaning MCL data.R (pipeline)
Output:  Clean data/mcl_main.Rda  (long: one row per post x district)
Written: July 2026. Reproduces end-to-end from Raw Data/MCL/*.csv.

--------------------------------------------------------------------------------
STAGE 0 - SETUP (once per run)
--------------------------------------------------------------------------------
1. GAZETTEER from lfs_dist_11.csv -- the only clean LFS frame. distname in
   lfs_dist_13/14 is CORRUPTED (increments across wards; invents "Quan 13/14/15"
   in HCMC). Admin prefixes stripped; keyed on (tinh, huyen). Districts created
   after 2011 (Bac/Nam Tu Liem, Lam Binh, Van Ho...) map to their 2011 PARENT so
   the panel merges with LFS codes. ~685 districts, 12 numbered (HCMC).
2. PROVINCE aliases (tphcm, sai gon, ha noi...) + UNIQUE-WARD list (wards in
   exactly one district nationally). Since July 2026 ward UNIQUENESS is
   judged on the COMPLETE GADM level-3 list (vnmap3, 11k wards) -- the LFS
   frame is not a comprehensive ward list, so LFS-based uniqueness could be
   FALSE (a ward repeating in a district the frame never listed). Names map
   to LFS-2011 codes via the gazetteer's canonical district names.
3. TEXT CANONICALIZATION (vn_canon), identical for posts and gazetteer:
   Unicode NFC, lowercase, tone-mark placement unified (hoa`->ho`a, gated so
   "hoang" is untouched), hard punctuation -> sentinel that blocks cross-clause
   matches. DIACRITICS ARE NEVER STRIPPED: folded, "Van Ban" = "van ban"
   (document), "Tuong Duong" = "tuong duong" (equivalent) -- hundreds of ghost
   matches per file.

--------------------------------------------------------------------------------
STAGE 1 - MATCHER (match_districts, per post)
--------------------------------------------------------------------------------
4. FOUR SCANS: district names (longest-first alternation, Unicode word
   boundaries); province tokens in text (positions kept); province tokens in
   the Page name; numbered districts (quan/q./district/dist 1-12, zero-padded,
   ALL occurrences -- first-only over-counted Quan 1-3; English "District 7"
   added July 2026, "distance" cannot fire).
5. GUARDS -- a hit is discarded when:
   - it lies inside a FIRM-NAME SPAN ("CONG TY CPCN VINH TUONG" names a company,
     not a place). Spans stop at punctuation, hiring verbs, and location markers
     (chi nhanh, KCN, so + digit, duong) so addresses after firm names survive.
     RESCUE: an out-of-span mention of the same district still counts.
   - a WARD MARKER precedes it ("phuong Thong Nhat" is a ward of Bien Hoa, not
     Huyen Thong Nhat).
   - for province evidence: a province token overlapping or identical to the
     district hit cannot vouch for it (34 provincial capitals share their
     province's name -- bare "Hue" must not become TP Hue).
6. EVIDENCE LADDER (first rung that holds wins; every row is tagged):
   prov_adjacent    its province within 40 chars (address triple)      HIGH
   numbered         Quan 1-12, HCMC                                    HIGH
   prov_in_text     its province elsewhere in the post        HIGH (MED if ambig)
   prefix_only      "huyen X", nationally unique name; the marker may  MEDIUM
                    also FOLLOW the name in English ads ("Binh Thanh
                    District"/"Dist", July 2026) -- diacritics still
                    required, so "Binh Thanh District" without tones
                    stays unmatched (diacritic-free matching rejected)
   prov_in_page     district in text; province from Page name          MEDIUM
   prefix_typed     AMBIGUOUS name resolved by its admin TYPE          MEDIUM
                    ("Q. Binh Tan" -> Quan Binh Tan HCMC, not Huyen
                    Binh Tan Vinh Long; q/quan, h/huyen, tx, tp; a
                    time like "8h." cannot become "huyen ..."); LOW
                    when a different province is stated; ranks below
                    prov_in_page and never displaces direct evidence
                    for the same district (added July 2026)
   prefix_conflict  prefixed but a DIFFERENT province stated           LOW
   bare             name alone, six gates: nationally unique, >=2      LOW
                    words, not on the corpus-learned "risky" list
                    (ly nhan < quan ly nhan su), not after a house
                    number, no contradicting province, not a capital
   The risky list is learned in a TWO-PASS run: pass 1 finds names that almost
   never carry prefix/province support (= ordinary words), pass 2 applies it.
7. FALLBACKS when text yields nothing: a marker-bearing unique WARD pins its
   district (MEDIUM; uniqueness on the complete vnmap3 list, see item 2);
   otherwise the post keeps PROVINCE ONLY (tinh set, huyen NA). A ward
   revoked by the stricter uniqueness may still fill via the ward3 pred tier
   (unique within a KNOWN province). The Page name NEVER supplies a district on its own -- it only
   disambiguates the province of a district named in the text (prov_in_page).

--------------------------------------------------------------------------------
STAGE 2 - RESHAPE
--------------------------------------------------------------------------------
8. One row per (post, district), ordered by first-mention position; a district
   named twice keeps its strongest evidence; district-less posts keep one row.
   n_districts = number of districts the post names. Engagement statistics live
   ONLY on post_row 1, so sums over the long table cannot double-count.

--------------------------------------------------------------------------------
STAGE 3 - JOB-LOCATION REFINEMENT
--------------------------------------------------------------------------------
9.  loc_cue: each district row classified by the cue preceding it within 150
    chars -- "work" (noi/dia diem lam viec, lam viec tai), "apply" (nop ho so,
    lien he), "hq" (tru so).
10. RE-RANK: the work-cue district is promoted to post_row 1 (stats follow),
    so the one-row-per-post view answers WHERE THE JOB IS. First mention
    already agreed with the work cue 94.7% of the time; this fixes the rest.

--------------------------------------------------------------------------------
STAGE 4 - INFERRED DISTRICTS (only for posts still district-less)
--------------------------------------------------------------------------------
OUTPUT CONVENTION (July 2026): all inferred tiers below write to the
SEPARATE columns dist_pred / huyen_pred / pred_source ("kcn" | "osr" |
"osrw" | "ward3") -- the main huyen/district columns hold ONLY districts
actually named in the post text. Precedence kcn > osr > osrw > ward3 (first
fill wins). KCN fills and national-pair osrw fills also set the main
tinh/province (a park or a nationally unique road+ward pair pins the
province unambiguously). Merge at analysis time:
    huyen_final := fifelse(!is.na(huyen), huyen, huyen_pred)
and report results with and without the inferred districts as robustness.
--------------------------------------------------------------------------------
11. KCN crosswalk: industrial park -> district, learned from posts naming both
    a park and a high-conf district (>=5 posts, >=65% agreement). vsip*
    blocklisted (multi-province chain); nomura pinned to Huyen An Duong (the
    learned mode was the recruiters' Kien An office). Tag "kcn", MEDIUM.
12. STREET crosswalk -- REMOVED (July 2026, user decision; see item 13).
    District inference now uses only EXTERNAL deterministic sources (KCN,
    OSM, GADM). The street EXTRACTION (RX_ST) survives: it feeds the OSM
    tiers' targets and corpus vetoes.
12b. OSM ROAD GAZETTEER (pred_source "osr"): external road list (UNION of the
    OSM snapshots under Raw Data/VNShapefile -- currently 2015-01-01 and
    2018-01-01 -- spatially joined to GADM LEVEL-3 polygons (vnmap3) at the
    top of the pipeline; the district is NAME_2 of that join, the ward NAME_3
    feeds item 12c) maps road names -> district. Denser snapshots both widen coverage
    and EXPOSE false within-province uniqueness (2015-only fills revoked by the
    2018 union: 22). Safeguards, in order:
      - GADM province/district names mapped to the 2011 LFS codes;
        highways (QL/TL/HL/cao toc, incl. diacritic-less "Quoc Lo") dropped;
        names kept only if 2-4 letter-words (the RX_ST extraction shape);
      - a road name must be UNIQUE within its province (multi-district
        names dropped);
      - road names equal to district/province names dropped (circularity);
      - CORPUS VETO: roads whose known-district posts disagree with OSM
        (>=3 posts, <60% agreement) are dropped -- kills multi-district
        arteries (Truong Chinh, Bach Mai...) whose within-province
        "uniqueness" is an artifact of partial OSM coverage.
    Uses the SAME address extraction as the street crosswalk (house number +
    name), fills only posts the learned crosswalk left NA, one implied district
    per post or nothing. Held-out validation: 96.5% on single-district-truth
    posts (89.2% when multi-district digest posts pollute the truth set; ~47%
    of residual errors are ADJACENT-district near-misses on boundary streets).
    8,697 roads (77 corpus-vetoed -- the 2018-union uniqueness gate already
    removes most multi-district arteries before the veto); fills 5,448 posts
    (3,926 job ads) now that the street crosswalk's posts fall to this tier.
    Re-verified July 2026 against the level-3 rebuild: 100% of fills
    reproduced, split-half held-out 96.8%.
    (Historical: the street-vs-osr head-to-head that preceded the street
    crosswalk's removal -- street 95.0% on 20,226 covered posts, osr 96.5%
    on 12,015, agreeing 99.3% where both answered; osr could replace ~35% of
    street's fills. Kept for the record; see item 13.)
12c. ROAD+WARD PAIRS (pred_source "osrw", July 2026): the OSM road union is
    now spatially joined to GADM LEVEL-3 (vnmap3), so every road carries the
    WARD it crosses. A (road, ward) pair identifies the specific stretch of a
    road, so it beats the road alone: held-out 98.6% (osr 96.5%), and a third
    of the 19k pairs lie on multi-district roads that osr's within-province
    uniqueness gate must discard. Two variants, both corpus-vetoed (>=3
    posts, <60% agreement):
      - (road, ward, province) key, fills posts with a known province;
      - NATIONALLY unique (road, ward) pairs (18.5k of 19.0k!) fill posts
        with NO province at all and set main tinh/province, like kcn
        (held-out: district 98.3%, province 99.3%).
    Ward mentions require a marker (phuong/xa/thi tran/p/tt); numeric wards
    ("P5") accepted with phuong/p only, digit-guarded so "30p"/times cannot
    fire. Street side reuses the RX_ST address extraction.
12d. PROVINCE-KEYED WARDS (pred_source "ward3", July 2026): vnmap3 is the
    complete ward list (11,163). A NAMED ward (>=6 chars, non-numeric,
    marker required) unique WITHIN ITS PROVINCE pins the district for posts
    whose province is known: held-out 98.3% on n=12,032. This extends the
    matcher's ward fallback (which demands NATIONAL uniqueness) without
    touching main columns; the "marker-less wards" rejection below still
    stands -- a marker is always required.
13. DELIBERATELY REJECTED after measurement (do not rebuild):
    - CORPUS-LEARNED STREET CROSSWALK (was item 12, removed July 2026 at the
      user's direction): (street, province) -> district learned from the
      corpus's own address patterns (>=5 posts, >=70% purity). Measured well
      (95.0% held-out, 3x osr's coverage; agreed with osr 99.3% where both
      answered) but is the one channel whose mapping comes from the corpus
      itself rather than an external source. Fills were dropped and refilled
      from osr/osrw/ward3 where those cover the same posts; the remainder
      reverted to district-NA.
    - MARKER-LESS WARDS EVEN WITH CONTEXT (measured July 2026): province
      known + ward anywhere = 53.8% precision (wards are named after job-ad
      vocabulary: lien quan, hop dong, phuc loi, thanh cong...); within 40
      chars of a province token = 82.2%; right after a house-number address
      = 90.8% (streets and wards share names: Lang Ha, Dinh Cong, Kim
      Giang...) for only ~254 job ads. All below the live tiers -- the
      MARKER stays mandatory.
    - diacritic-free matching: pure noise (van ban, tuong duong, la gi...)
    - capital imputation: bare "Bac Ninh" means the province, not TP Bac Ninh
    - marker-less wards: "an ninh" (a ward) means "security" -> every guard ad
    - phone-prefix -> province: mechanism validated (prefixes reproduce real
      area codes; mobiles fail dominance) but yield tiny (~2,200 provinces)
    - PAGE-HISTORY inference (fill a page's district-less ads from where its
      other located ads point): ~94% on audit but NO TIME DIMENSION -- firms
      move, so a relocated firm stamps its OLD district on new posts, with
      errors correlated within page. Streets and parks do not move, which is
      why those crosswalks stay and this one does not.
    - district_in_page (Page named "Viec lam Cau Giay" -> Cau Giay): removed
      from the matcher entirely for the same reason as page-history -- it
      infers location from the POSTER, not the post; page names are often
      personal names ("Le Chi Linh" -> TX Chi Linh) and fills cluster on a
      few prolific pages.
    - GLMNET DISTRICT PREDICTION (per-province lasso multinomial logit on
      TF-IDF text, trained on the matcher's own assignments; was "Stage 4b",
      July 2026): held-out accuracy 82% top-1, 95.6% at prob>=0.8 -- but only
      ~8% of NA posts cleared that bar, and predictions rested partly on
      brand/firm-name correlations (imputation-flavored). Dropped in favour of
      the deterministic OSM road gazetteer (item 12b), which reads the same
      residual address signal transparently. Before removal it independently
      AGREED with road_gaz on 94.6% of overlapping fills (prob>=0.8) --
      retained here as corroborating evidence for the road tier.

--------------------------------------------------------------------------------
STAGE 5 - PENDING (external inputs)
--------------------------------------------------------------------------------
- mst / mst_branch: 10-digit tax codes printed in posts (992 posts, 505 firms;
  cue-anchored so phone numbers are never captured; keep as CHARACTER -- leading
  zeros matter). 166 district-less posts can take the firm's district from the
  Vietnam Enterprise Survey (merge on the 10-digit base; tag "mst_es"; note ES
  address = registered HQ, not necessarily the worksite).
- MCL producer/Page address: if the Meta Content Library data dictionary
  exposes a Page's declared address, a "page_addr" tier would supersede
  page-history (declared beats inferred). Unverified.

--------------------------------------------------------------------------------
CURRENT COVERAGE (July 2026, .Rda state; is_job_ad v10 canthoinfo poster+text
= 298,389 job ads; user full rerun + prefix_typed/District-N splice applied)
--------------------------------------------------------------------------------
All posts w/ TEXT-NAMED district .. 172,510 / 387,909  (44.5%)
JOB ADS (is_job_ad==1):
  text-named district (huyen) .... 157,400 / 298,389  (52.7%)
    of which conf HIGH ........... 121,313
    + MEDIUM ..................... 144,551  (incl. 2,397 ward-fallback)
    + LOW ........................ 157,400
  inferred (pred_source) ......... kcn 983 | osr 3,808 | osrw 474
                                   | ward3 228  (street channel REMOVED;
                                   its refills agreed with street 96.9%)
  text-named + inferred .......... 162,893  (54.6%)
District NA but province known .... 43,403  (district OR province: 69.1%)
July 2026 splices: typed-prefix/District-N (+688 posts; 393 English
"District N", 319 "Q./H./TX/TP + ambiguous name"); 2 legacy Lam Binh
NA-rows repaired (parent now correctly Huyen Na Hang); ward fallback made
LIVE (its rows had been dropped at the reshape since the start -- "ward"
was missing from NAMED_IN_TEXT) with uniqueness from the complete vnmap3
list: +2,998 ward-evidence posts.

--------------------------------------------------------------------------------
HOW TO CONSUME
--------------------------------------------------------------------------------
- post_row == 1 is THE job's district (one row per post; stats live here).
- huyen/district = TEXT-NAMED only. Inferred districts live in dist_pred /
  huyen_pred with pred_source in {"kcn","osr","osrw","ward3"}; to use them:
    huyen_final := fifelse(!is.na(huyen), huyen, huyen_pred)
  Run results with and without huyen_pred as the built-in robustness check;
  pred_source lets you drop any single inference channel.
- Recommended spec (text-named): dist_conf %in% c("high","medium").
- Gold check on loc_cue == "work"; province-level spec on tinh (~68%).
- A row means "this post names this district", not proof the job is there;
  n_districts flags multi-district digests (consider n_districts <= k).
- Zeros are one-sided: no district found != job has no district.
- Held-out accuracy of the inferred tiers (single-district-truth protocol):
  osr 96.8%, osrw 98.6% (pairs; national variant 98.3% district / 99.3%
  province), ward3 98.3%; kcn was audited and repointed after audit. The
  matcher tiers rest on the documented guards + design audits.
================================================================================
