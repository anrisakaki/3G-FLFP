================================================================================
JOB AD IDENTIFICATION (is_job_ad) -- how posts are classified
================================================================================
Applies to: mcl_main (Clean data/mcl_main.Rda), built by "Cleaning MCL data.R".
The definition lives in the JOBAD_* block of "Cleaning MCL data.R" (~lines 24-90).
Related variable: job_page (poster is a dedicated job/recruitment page).

Context: the corpus is already pre-filtered to posts whose text contains
"tuyen dung" (tuyển dụng), so is_job_ad separates ACTUAL job ads from other
content that merely mentions recruitment (news, CV tips, course promos,
contests, jokes, admin notices).

All matching runs on canonicalised text: Unicode NFC, lowercased. Diacritics
are never stripped.

--------------------------------------------------------------------------------
STEP 1 -- score the post text against three signal lists
--------------------------------------------------------------------------------
MAIN families (JOBAD_MAIN, 11 families; each counts at most once):
   1. Job description   mô tả (chi tiết) công việc, nội dung công việc, công việc:
   2. Requirements      yêu cầu:, yêu cầu công việc/ứng viên/chung/tuyển
   3. Benefits          quyền lợi, đãi ngộ, phúc lợi
   4. Salary            mức lương, lương:, lương cứng/cơ bản/căn bản/cb/khởi
                        điểm/từ N/thỏa thuận, lương + digits, thu nhập
   5. Headcount         số lượng: N, số lượng tuyển/cần
   6. Position header   vị trí:, vị trí tuyển/công việc/cần, chức danh, chức vụ
   7. Application       nộp/gửi hồ sơ, hồ sơ gồm/xin việc, ứng tuyển,
                        hạn chót/nộp, deadline
   8. Working hours     thời gian làm việc, giờ làm, ca sáng/chiều/tối/xoay/
                        gãy/đêm, theo ca, full time, part time
   9. Workplace         nơi/địa điểm/địa chỉ làm việc, làm việc tại
  10. Contact           liên hệ/hotline/zalo/sđt/đt + digits within 20 chars;
                        standalone phone numbers, incl. dotted/spaced forms
                        (0167.33.55555, 0912 345 678)
  11. Hiring verb+role  cần tuyển, tuyển gấp, thông báo tuyển dụng, tin tuyển
                        dụng, or tuyển (dụng) followed within ~8 non-letter
                        chars (emoji, ":", "1/", numbering OK) -- or on the
                        NEXT LINE as a numbered list item ("TUYỂN DỤNG:\n1. ...")
                        -- by a role from JOBAD_ROLE (~45 roles: nhân viên, nv,
                        thợ, ctv, kế toán, công nhân, kỹ/kĩ sư, kỹ thuật (viên),
                        lập trình viên, tester, developer, designer, pha chế,
                        bán hàng, sales, lễ tân, tài xế, nam, nữ, người, bạn,
                        vị trí, ...); also "cần (gấp) (N) + role" without tuyển

PERIPHERAL signals (JOBAD_PERI, 3): bare "địa chỉ:", salary amounts
  ("5tr/tháng", "20k/h"), ib/inbox. They count toward the signal total but can
  NEVER carry a post alone -- they are also the anatomy of a product-sales post.

NEGATIVE families (JOBAD_NEG, 5): CV/interview tips (bí quyết, mẹo, cách viết/
  trả lời, kinh nghiệm phỏng vấn); "nhà tuyển dụng" discussion; courses &
  enrolment (khai giảng, học phí, khoá học, tuyển sinh, học bổng); news/
  reporting language (phóng viên, theo báo, cho biết, khẳng định); contests &
  events (cuộc thi, hội thảo, workshop, minigame, giveaway).

--------------------------------------------------------------------------------
STEP 2 -- a post is a job ad if it passes ANY of four paths
--------------------------------------------------------------------------------
PATH 1, base rule (the workhorse; a structured ad hits 4-8 families):
      total signals >= 2
  AND main signals  >= 1
  AND NOT (negative families >= 2 while total signals <= 3)

PATH 2, job-page rescue: the poster is a dedicated job page (see job_page
  below). These posts need only ONE main text signal (instead of two),
  provided zero negative and zero rescue-veto hits.

PATH 3, strong-keyword rescue: one unambiguous hiring phrase suffices BY
  ITSELF -- thông báo tuyển dụng, (thông) tin tuyển dụng, vị trí tuyển dụng,
  cần tuyển, tuyển gấp, tuyển (+ role) -- but only if its FIRST occurrence
  starts within the first 250 characters of the post (JOBAD_STRONG + onset
  gate), and zero negative / rescue-veto hits. Rationale: in real ads the
  phrase is the header (image ads with caption-only text, classifieds where
  an address precedes the hiring line); when it appears deep in the text it
  is boilerplate inside news stories, brand trivia, or volunteer roundups.

PATH 4, job-hashtag rescue: a recruitment hashtag anywhere in the post suffices
  BY ITSELF (JOBAD_HASH), with zero negative / rescue-veto hits. Tags matched
  (diacritics optional; compound tags allowed, e.g. #tuyendungbachhoaxanh):
    #jobs (exact tag), #tuyendung / #tuyểndụng, #tintuyendung / #tintuyểndụng,
    #vieclam / #việclàm.
  Unlike Path 3 there is NO 250-char onset gate, because these tags almost
  always sit in the FOOTER of a post. "#jobs" is held to an exact tag ("job"
  alone is too ambiguous); the Vietnamese tags allow trailing characters so
  concatenated compound tags still match. Catches terse relay posts and image
  captions (job boards, employer pages) whose body text is too thin for Path 1.

--------------------------------------------------------------------------------
STEP 3 -- rescue-only vetoes (JOBAD_NEG2)
--------------------------------------------------------------------------------
Paths 2, 3 and 4 are additionally blocked by a veto list built from audited
false positives on job pages:
  - promotions:      khuyến mãi, giảm giá, ưu đãi, voucher, sale off
  - jokes:           tuyển chồng/vợ/người yêu/bạn trai/bạn gái
  - admin notices:   "muốn/cần gửi/đăng tin ...", "đăng tin tuyển dụng vui lòng"
  - scam warnings:   lừa đảo, cảnh báo, giả mạo, bóc phốt, đa cấp
These vetoes deliberately do NOT touch the base rule (Path 1), so tightening
the rescues can never un-flag a structured ad.

--------------------------------------------------------------------------------
job_page variable
--------------------------------------------------------------------------------
job_page = 1 when the poster's identity (surface.name + post_owner.name +
surface.username, canonicalised) matches JOBAD_PAGE:
  - việc làm / tìm việc / tuyển dụng, with or without diacritics and spaces
    (so usernames like "vieclam24h", "tuyendungdaklak" count)
  - word-boundary "job" (jobs, jobsgo, jobstreet) or "hr" (HR Insider, HRC)
  - known job-board brands: vietnamworks, career*, topcv, mywork
Covers ~34% of posts (~2,800 distinct pages). Kept as its own column for
analysis; constant within post id.

--------------------------------------------------------------------------------
Version history (each version is a strict superset of the previous: posts are
only ever ADDED, never removed, so counts are monotone)
--------------------------------------------------------------------------------
v1  234,632 ads (60.5% of posts)
    >=2 of 11 families, minus negative veto. Too conservative: missed terse
    header-style ads, dotted phone numbers, "cần gấp + role", kĩ-spelling.
v4  262,067 (67.6%)
    Families widened (all strict supersets of v1); peripheral trio added but
    demoted so it can't fire alone; role list extended; numbered-list and
    next-line headers handled. Intermediate v2/v3 imposed a "core signal"
    requirement that was AUDITED AND REJECTED: it dropped ~5,300 posts of
    which ~80% were real ads.
v6  295,222 (76.1%)
    Job-page rescue (Path 2) + strong-keyword rescue with 250-char onset gate
    (Path 3) + rescue-only veto list. 90.5% of district-located posts are now
    flagged; job ads with a district: 159,703 (54.1% of job ads).
v7  296,788 (76.5%)   <- CURRENT
    Job-hashtag rescue (Path 4): #jobs / #tuyendung / #tintuyendung / #vieclam,
    diacritics optional, no onset gate, veto-gated. +1,566 posts, all terse
    relay/caption ads (job boards, employer pages). Job ads with a district:
    159,870 (53.9% of job ads).

--------------------------------------------------------------------------------
Quality control
--------------------------------------------------------------------------------
- Every revision was measured read-only on the full corpus first, with random
  samples of newly-included and newly-excluded posts adjudicated by hand.
- Applies are gated on unit tests: adjudicated real posts pinned to 1 (Senla
  cafe, OMINEXT "KĨ SƯ", Lemon Tree Spa, dotted-phone restaurant ad, ...);
  synthetic product posts / scam warnings / jokes / late-boilerplate posts
  pinned to 0. Any failure aborts without saving.
- The .Rda and the pipeline block are verified to reproduce each other with
  zero mismatches across all rows.
- Known residual false positives (kept deliberately -- removal would cost real
  ads): course promos and interview-tips posts that quote salaries and pass
  the base rule. RA labels will quantify precision.
================================================================================
