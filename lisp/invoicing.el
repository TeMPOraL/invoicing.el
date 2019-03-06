;;; invoicing.el --- Generate and manage invoices with Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jacek Złydach

;; Author: Jacek Złydach <temporal@temporal.pl>
;; Keywords: Org, agenda

;; TODO License

;;; Commentary:

;; TBD.


;;; Code:
;;;; Dependencies
;;;; TODO make them also visible in the comment header, per Emacs package commenting style.
(require 'cl-lib)
(require 'org)
(require 'org-table)
(require 'dash-functional)

;; TODO code in general

;; config definitions -- TODO separate out to another file
;; XXX ALSO SOME OF THOSE MAY BE PER COMPANY - WHAT IF USER WANTS TO HAVE MULTIPLE COMPANIES (e.g. their own company, and hackerspace they run)?
(defgroup invoicing nil
  "Settings for `invoicing'."
  :tag "Invoicing"
  :group 'org
  :link '(url-link "https://github.com/TeMPOraL/invoicing.el"))

;TODO could this be locally overridable per customer?
;TODO a default value could be provided, relative to this library's folder.
(defcustom invoicing-template-directory nil
  "Path to a folder containing invoice templates.
This path will be used as the root when looking up templates specified
with relative paths."
  :group 'invoicing
  :type 'directory)

;;; TODO could it be locally overridable per customer?
(defcustom invoicing-archive-directory nil
  "Path to a folder containing archived invoices.
This folder should store invoices that have already finished
their accounting life cycle. Contents of this folder are only
listed in order to determine the next applicable invoice number."
  :group 'invoicing
  :type 'directory)

;;; TODO could it be locally overridable per customer?
(defcustom invoicing-staging-directory nil
  "Path to a folder in which to store generated invoices.
Invoices in this folder are yet to be sent to the customer,
or yet to be sent to the accounting people."
  :group 'invoicing
  :type 'directory)


;;(defcustom invoicing-warning-)


;;; XXX AROUND HOOK
;;; Probably the simplest way to allow specific overrides would be to provide ability
;;; to register one or more around hooks - a functions of form:
;;; (lambda (next-function)
;;;   ;; do :before stuff
;;;   (funcall next-function)
;;;   ;; do :after stuff
;;; )
;;; The idea here being to give the ability to change the dynamic environment around settings
;;; and selections - e.g. rebind current customer, current seller, and possibly other settings,
;;; based on whatever is selected. This way, we stuff as much configuration as possible into
;;; the customer & seller definitions, and allow altering them in special cases.
;;;
;;; #emacs suggests going with explicit defvar if I expect 10% or more users to want to use it, for discoverability.

;;; XXX DATA ORGANIZATION TO RETHINK
;;; In context of both user-config and hook-overriding.
;;; Should I start the generation process with "all data normalized"? I.e. any piece of information
;;; needed by the process is in _one_ place?
;;; That would need to have more in context than just current-customer and current-seller; hell, it'll
;;; probably require flattening this into a single data block.


;;; XXX DATA DEFINITIONS
;;; This is a local reference, not something that'll be part of final project code.
;;; Actually, it might be worth to define some of them as defcustom, and others as def-whatever datatypes are appropriate.
;;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html for defcustom.
;;; See https://nullprogram.com/blog/2018/02/14/ for structures.
;;;
;;; INPUTS
;;; User's company configuration
;;; - Full name
;;; - Address lines
;;; - Tax ID (optional)
;;; - Payable days default?
;;; - Accounts - a map of shortcode :: account definition
;;; - Default currency
;;;   Account definition:
;;;   - Name
;;;   - Bank name and address
;;;   - Account number
;;;   - SWIFT (optional)
;;;   - Account currency override [default default]
(define-widget 'invoicing-address 'lazy ;XXX why 'lazy works, and other stuff doesn't?
  "TODO document"
  :tag "Address"
  :type '(repeat :tag "Address lines" (string :tag "Line"))) ;TODO Figure out a way to get rid of excess "Repeat:" in customize view.

(define-widget 'invoicing-account 'alist
  "TODO document"
  :type '(alist :key-type symbol)
  :tag "Bank account"
  :options '((:shortcode string)
             (:bank invoicing-address)
             (:number string)
             (:currency string)         ;XXX what is the meaning of this?
             (:swift string)))

(define-widget 'invoicing-tag 'alist
  "TODO document"
  :type '(alist :key-type symbol)
  :options '((:tag string)
             (:name string)
             (:rate integer)
             (:vat-rate (choice (number :tag "VAT percent rate") ;XXX display [%] instead of "percent rate"
                                (string :tag "Special VAT type")
                                (const :tag "No VAT applicable" nil)))))

(define-widget 'invoicing-seller 'alist
  "Definition of a single invoicing identity.
Here you can specify the data that goes into \"seller\" field
on the invoice, as well as override some of the global invoicing
settings."
  ;; XXX need something to unbreak widget alignment in the UI!
  :tag "Seller"
  :type '(alist :key-type symbol)
  :options '((:shortcode string)
             ((const :tag "A Full Name" :full-name) string) ;XXX Turns out :doc "documentation" works too, albeit with no indent :/.
             (:address invoicing-address)
             (:tax-id string)
             (:language string)         ;"Home" language code. XXX string?
             (:accounts (repeat invoicing-account))))

(define-widget 'invoicing-customer 'alist
  "TODO Docstring"
  :tag "Customer"
  :type '(alist :key-type symbol)
  :options '((:shortcode string)
             (:full-name string)
             (:language string)         ;"Remote" language code. XXX string?
             (:currency string)         ;Currency to bill in.
             (:agenda-paths (repeat directory))
             (:address invoicing-address)
             ;; TODO account selector - maybe a list of strings matching shortcodes? a currency-based selector? a function of (seller, customer) => account?
             (:invoice-template file)   ;XXX file? maybe a string? or a choice between the two? should prefer relative paths!
             (:tags (repeat invoicing-tag))
             (:tax-id string)
             (:reverse-charge boolean)
             (:payable-days integer)
             (:block string)))

(defcustom invoicing-broken-widget '()
  "Testing broken widget."
  :group 'invoicing
  :type 'invoicing-address)

;;; TODO figure out how to better document individual alist fields in customize!




(defcustom invoicing-sellers '(((:shortcode . "TST1")
                                (:full-name . "ASDF")
                                (:address . ("Line1" "Line2" "Line3"))
                                (:accounts . (((:shortcode . "ACC1")
                                               (:bank . ("Line1.1" "Line1.2")))
                                              ((:shortcode . "ACC2")
                                               (:bank . ("Line2.1" "Line2.2")))))
                                (:some-other-key . "asdf")))
  "A list of identities used to invoice other parties.
At least one is needed for Invoicing to operate.
If you invoice in the name of multiple organizations,
this is the place to specify each of them.
TODO further documentation."
  :group 'invoicing
  :type '(repeat invoicing-seller))
;;;
;;; Customer configuration
;;; - Shortcode?
;;; - Language [default default]
;;; - Currency used to bill them (why in my prototype there's both :currency and :billing-currency"?) [default default]
;;; - Agenda paths (optional)
;;; - Full Name (for Invoice)
;;; - Address Lines
;;; - Target account selector [default first]
;;; - Invoice template name [default default]
;;; - Tax ID [default none]
;;; - Reverse charge remarks [default nil, or default default]
;;; - Payable days [default default]
;;; - Block? [default default]
;;; - email maybe? for automated sending of invoices. That's probably beyond MVP though.



(defcustom invoicing-customers '(((:shortcode . "CTST1")
                                  (:agenda-paths . ("/home/temporal/repos/invoicing.el/test"))
                                  (:tags . (((:tag . "test_one")
                                             (:name . "Test tag 1")
                                             (:rate . 42)
                                             (:vat-rate . 23))
                                            ((:tag . "test_three")
                                             (:name . "Test tag 3")
                                             (:rate . 12)
                                             (:vat-rate . 18))))))
  "TODO documentation"
  :group 'invoicing
  :type '(repeat invoicing-customer))
;;;
;;; Tags configuration - a map of tag name :: tag definition
;;; - Line item name
;;; - Hourly rate
;;; - Tax rate (if applicable)
;;;
;;; RUNTIME INPUTS TO LINE ITEM TABLE GENERATION
;;; Time sum data - this will be passed to each column descriptor for generation of a column
;;; - Tag with full set of properties (if going by tag selection, default implementation)
;;; - Hours collected
;;;
;;; Column descriptor
;;; - Colum name
;;; - Column variable name (for named tblfms)
;;; - A function that turns Time sum data instance into value for this column
;;; - A function that turns a list of Column descriptors into a list of TBLFMs to add to line-item table.
;;;
;;; Globals
;;; - Receiving currency
;;; - Currency conversion rates (if different taxable currency?)
;;;
;;; XXX how to describe/handle VAT subtotals?
;;;
;;; ALSO TO DEFINE
;;; Translation data?
;;; - Maybe we can get away with having a translation list for language, + a bilingual flag?
;;; - Translations would be done as either string, or (lambda (contex) ...) -> string
;;; - Also each language needs to have number->words function defined.
;;;
;;; Tax data?
;;; - I think this can be handled by a combination of Column descriptors and Company/Customer configurations
;;;   (Might possibly want to just dump combined KVs into org mode table, with list of excluded keywords.)
;;;
;;; Post processors for template generation?
;;; E.g. latexification of stuff, like \nth in addresses.


;;; General utils FIXME move to a different file

(defun invoicing--agetter (key)
  "Return an alist-getter for `KEY'.
An alist-getter is a function that accepts an alist,
and returns the value associated with `KEY'."
  (-partial #'alist-get key))



(defun invoicing--completing-read-by-shortcode (list &optional code prompt)
  "Select entry from `LIST' (`SELLER' or `CUSTOMER') by shortcode.
Use `CODE' as the default shortcode value.
If specified, `PROMPT' will be used as prompt for completing read.
Returns a whole element from the list, not just its shortcode."
  (let ((codes (mapcar (invoicing--agetter :shortcode) list)))
    (completing-read (or prompt "Shortcode: ") codes nil t nil nil code)))

;;; OK, so here I'm trying to normalize the data needed to render a single invoice.
;;; The shape of this suggests the following workflow.
;;; 1. User selects customer + seller, this + global settings build the normalized data set.
;;; 2. That normalized data set is then updated with data collected from Org Agenda and serialized (almost) wholesale into the Org document.
;;;    (Except functions. Those aren't.)
;;; 3. The new data structure is pulled from Org Mode document and transfered to Invoice Generator.
(defun invoicing--collect-worksheet-context (seller-data customer-data account-data)
  "TODO Normalized data, showing clearly what comes from where."
  (let ((seller-currency (alist-get :currency account-data))
        (customer-currency (alist-get :currency customer-data)))
    `(
      ;; --- Seller specific ---
      (:seller-shortcode . ,(alist-get :shortcode seller-data))
      (:seller-full-name . ,(alist-get :full-name seller-data))

      (:seller-address . ,(alist-get :address seller-data))
      (:seller-tax-id . ,(alist-get :tax-id seller-data))

      ;;:language "???" ;"Home" language code. XXX string?

      (:seller-account-shortcode . ,(alist-get :shortcode account-data))
      (:seller-bank-address . ,(alist-get :bank account-data))
      (:seller-account-number . ,(alist-get :number account-data))
      (:seller-account-currency . ,seller-currency)
      (:seller-account-swift-code . ,(alist-get :swift account-data))

      ;; --- Customer specific ---
      (:customer-shortcode . ,(alist-get :shortcode customer-data))
      (:customer-full-name . ,(alist-get :full-name customer-data))
      ;;(:language . "??")         ;"Remote" language code. XXX string?
      (:customer-currency . ,customer-currency)
      ;;(:agenda-paths (repeat directory))
      (:customer-address . ,(alist-get :address customer-data))
      ;;(:invoice-template file)   ;XXX file? maybe a string? or a choice between the two? should prefer relative paths!
      (:customer-tax-id . ,(alist-get :tax-id customer-data))

      ;; --- For tag-based summing
      ;; -- stuff below gets replaced by org mode table --
      (:tags . ,(alist-get :tags customer-data)) ;TODO merge with seller and default setup

      ;; (:tags . (((:tag "Tag1")
      ;;            (:name  "Line item description")
      ;;            (:rate 42)
      ;;            (:hours nil) ;Hours will be appended by the Org collection process.
      ;;            ;; Also other data may show up here, if column generators create it.
      ;;            )
      ;;           ((:tag "Tag2")
      ;;            (:name "Line item description 2")
      ;;            (:rate 12)
      ;;            (:hours nil))))

      ;;XXX introduce columns-by-shortcode fetching
      ;;(:columns . ,(invoicing--compute-line-item-table-columns nil))
      ;; -- end of stuff that gets replaced by org mode table --

      (:currency-conversion . ,(unless (eql seller-currency customer-currency)
                                 (invoicing--get-currency-conversion-rate seller-currency customer-currency)))

      ;; --- Customer&Seller ---
      (:payable-days . 14)
      (:reverse-charge . t)
      (:block . ,(or (alist-get :block customer-data) (alist-get :block seller-data) 'lastmonth)) ;XXX also default block?

      (:agenda-paths . ,(or (alist-get :agenda-paths customer-data) (alist-get :agenda-paths seller-data))) ;XXX also default agenda-paths?
      ;; XXX also use global default paths for both?
      (:invoice-template . ,(let* ((template-basedir (or (alist-get :template-basedir customer-data) (alist-get :template-basedir seller-data)))
                                   (template-filename (or (alist-get :template-file customer-data) (alist-get :template-file seller-data) "FIXME invoicing.el-default filename")))
                              (if (file-name-absolute-p template-filename)
                                  filename
                                (expand-file-name template-filename template-basedir))))
      (:generating-engine . :LaTeX)     ;XXX a function maybe?

      ;; --- Other ---
      ;;(:invoice-number . "TODO Invoice Number") -- XXX remove, will be generated in metadata block
      ;;(:issue-date . "TODO date") -- XXX remove, will be generated in metadata block
      ;;(:sale-date . "TODO date") -- XXX remove, will be generated in metadata block
      ;;(:period . "TODO date") -- XXX remove, will be generated in metadata block

      ;; --- Mechanics ---

      ;; XXX see if current org clocksum algo doesn't already account for it
      (:warn-on-multiple-tags .  t) ; if we have more than one of the counted tags in effect on a headline (possibly via inheritance)
      (:warn-on-other-tags .  t)
      (:warn-on-unclassified-time . t)
      (:include-unclassified-time . t)
      (:include-summary-by-headline . t)
      (:include-copy-of-line-items . t)
      (:include-help-text . t)
      )))


;; taxinfo code -- TODO separate out to another file


;; source-org code -- TODO separate out to another file

;; TODO implementation needs to be able to go over all org-agenda files,
;; discover all headlines with clock data, and aggregate them by tag.
;; The implementation needs to pay attention to entries with multiple customer tags,
;; with tags that are not on the customer list, and with entries that have no tags.
;; We have to have reports on that.
;;
;; With that in mind, let's agree on an interface allowing for all this, and then
;; for MVP just implement the old tag matching I did. After MVP, we can extend
;; this to support mistaggings and non-taggings correctly.
;; Note that even for MVP, we can probably issue warnings about multiple tags for an item.
;; Also, we could probably compute the diff for tags vs. no tags in file, for one extra file pass.
;;
;; TODO also maybe defcustom for whether we report unclassified time as Other column, just as warning,
;; or Other + warning.
;;
;; NOTE we can probably use org-map-entries to iterate over file and grab headlines with tags.

;; (defun trc-invoicing--compute-time-for-tag (tag-name block)
;;   "For a given `TAG-NAME', compute a sum of time spent on it in agenda files within a time `BLOCK'."
;;   (let ((total 0))
;;     (mapc (lambda (file)
;;             (let* ((params `(:tags ,tag-name :block ,block))
;;                    (clock-data (with-current-buffer (find-file-noselect file)
;;                                  (org-clock-get-table-data (buffer-name) params))))
;;               (incf total (/ (nth 1 clock-data) 60.0))))
;;           (org-agenda-files))
;;     total))

(defun invoicing--org-compute-tags-sum (params)
  "TODO document"
  ;; Return tag sums, + warnings about misclassified items, + untagged time for block.
)


;; worksheet generator code -- TODO separate out to another file

(defun invoicing--find-by-shortcode (value seq)
  "Find item in `SEQ' with shortcode `VALUE'."
  (cl-find value seq :key (invoicing--agetter :shortcode) :test #'equalp))

;;; Context variables.
(defvar-local invoicing-worksheet-context nil
  "Context for currently open worksheet.
This variable stores an alist completely describing an invoice for purposes of
generating a worksheet. Code for interactive tables will refer to this variable
for all necessary metadata.")

(defun invoicing--ensure-worksheet-context (&optional seller customer account force force-ask)
  "Ensure proper context exists for current worksheet.
Ensures `INVOICING-WORKSHEET-CONTEXT' contains full set of data
required by the worksheet. If `INVOICING-WORKSHEET-CONTEXT' is empty,
user will be prompted to specify seller, customer and account combination to
generate data anew. `SELLER', `CUSTOMER' and `ACCOUNT' values can be provided
as defaults. If `FORCE' is not nil, context will be regenerated even
if it already exists. If `FORCE-ASK' is T, the user will be prompted
for new values even if `SELLER', `CUSTOMER' and `ACCOUNT' are given."
  (cl-flet ((ensure-item (list default-code name)
                         (let ((code (or (and default-code (not force-ask))
                                         (invoicing--completing-read-by-shortcode list default-code (format "%s: " (capitalize name))))))
                           (or (invoicing--find-by-shortcode code list)
                               (error "Invoicing: invalid or unspecified %s" name)))))
    (when (or (null invoicing-worksheet-context)
              force)
      (let* ((seller-data (ensure-item invoicing-sellers seller "seller"))
             (customer-data (ensure-item invoicing-customers customer "customer"))
             (seller-accounts (alist-get :accounts seller-data)) ;TODO maybe also support "global" invoicing-accounts?
             (account-data (ensure-item seller-accounts account "account")))
        (message (buffer-name (current-buffer))) ;XXX debug
        (setf invoicing-worksheet-context
              (invoicing--collect-worksheet-context seller-data customer-data account-data)))))
  (values))

(defun invoicing--get-in-context (key &optional default nil-as-missing)
  "Get value associated with `KEY' in `INVOICING--GET-IN-CONTEXT'.
Accepts `DEFAULT' if value is not found. Unlike `ALIST-GET', it treats
`NIL' value for an existing key as missing value."
  (or (alist-get key invoicing-worksheet-context default)
      default))

(cl-defstruct (invoicing-column-descriptor
               (:constructor nil)
               (:constructor invoicing-make-column-descriptor (name tag value-generator tblfm-generator))
               (:copier nil))
  "Line-item table column descriptor.
Each instance provides data fully configuring a single column in the line-item table
(XXX except special columns for currency conversions & stuffs.)
Slots:
- `NAME' - TODO
- `TAG' - TODO
- `VALUE-GENERATOR' - TODO
- `TBLFM-GENERATOR' - TODO"
  (name (error "Name not specified") :read-only t)
  (tag (error "Tag not specified") :read-only t)
  (value-generator (error "Value generator not specified") :read-only t)
  (tblfm-generator (error "TBLFM generator not specified") :read-only t))

(defvar invoicing-additional-columns nil) ;TODO also probably should be defcustom, and/or overridable at customer or seller-level.

;;; NOTE XXX Simplifying tablegen
;;; We assume three groups always exist (but may be empty):
;;; @I..@II - line items
;;; @II..@III - VAT subtotals
;;; @> - Total
;;; We also always include recalc column. The primary parametrization comes from currencies in column labels, and optional inclusion of VAT columns.
;;; NOTE XXX Extra simplification: we'll drop subtotals for the time being, as I can't figure out TBLFMs that would be robust under editing
;;; (one apparently cannot use hline-relative references as lvalues)
;;; NOTE maybe we can hack it with https://emacs.stackexchange.com/questions/15185/cant-assign-to-hline-relative-reference, but it won't look pretty

(defun invoicing--compute-line-item-table-columns (params) ;XXX maybe get rid of the params argument?
  "TODO document"
  `(;; TODO if possible, move that to defcustommable variable?
    ,@(mapcar (-partial #'apply #'invoicing-make-column-descriptor)
              `((" " " " ,(-const "#") ,(-const nil))
                ("Tag" "Tag" ,(invoicing--agetter :tag) ,(-const nil))
                ("Line item name" "LineItem" ,(invoicing--agetter :name) ,(-const nil))
                ("Hours" "Hours" ,(-compose (-partial #'format "%12.6g") (-rpartial #'/ 60.0) (invoicing--agetter :time)) ,(-const `("TODO tblfm for total Hours")))
                ;; TODO format currency - potentially extensible!
                ("Rate" "Rate" ,(invoicing--agetter :rate) ,(-const `("TODO tblfm for total Rate")))
                ("Net" "Net" ,(-const nil) ,(-const `("TODO tblfm for row Net" "TODO tblfm for total Net")))))
    ,@invoicing-additional-columns
    ,(invoicing-make-column-descriptor "Gross" "Gross" (-const nil) (-const `("TODO tblfm for row Gross" "TODO tblfm for total Gross")))))

(defun invoicing--compute-line-items-table (params)
  "Actual computations for line item table.
TODO documentation
TODO document `PARAMS' or refer to dblock fun."
  (let* ((org-agenda-files (invoicing--get-in-context :agenda-paths))
         (columns (invoicing--compute-line-item-table-columns params))
         (headers (mapcar #'invoicing-column-descriptor-name columns))
         (tblfms (-mapcat (-compose (-rpartial #'funcall columns) #'invoicing-column-descriptor-tblfm-generator) columns))
         (selected-tags (invoicing--get-in-context :tags))
         (all-tags-in-files)            ;TODO collect them all
         (total-time-in-files 0)        ;TODO collect it
         (block (invoicing--get-in-context :block))
         (sums)
         (warnings)
         (unclassified-time))

    ;; Collect total time in files, and all used tags.
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        ;; FIXME inefficient; we do N+2 scans of each agenda file; we could likely collect all this data manually in a single scan.
        (setf all-tags-in-files (append all-tags-in-files (mapcar #'car (org-get-buffer-tags))))
        (incf total-time-in-files (nth 1 (org-clock-get-table-data (buffer-name) `(:block ,block))))
        ;; TODO make it also count time for _other_ tags, in order to warn only on other tags that have time assigned to them.
        (dolist (tag-entry selected-tags)
          ;; FIXME rewrite to make it clean
          (let ((time (nth 1 (org-clock-get-table-data (buffer-name) `(:tags ,(alist-get :tag tag-entry) :block ,block))))
                (sums-entry (cl-find (alist-get :tag tag-entry) sums :key (invoicing--agetter :tag) :test #'equalp)))
            (if sums-entry
                (incf (alist-get :time sums-entry) time)
              (push (append tag-entry `((:time . ,time)))
                    sums))))))

    (setf all-tags-in-files (cl-remove-duplicates all-tags-in-files :test #'equalp))

    (let ((total-time-in-tags (cl-reduce #'+ sums :initial-value 0 :key (invoicing--agetter :time))))
      (unless (= total-time-in-tags total-time-in-files)
        (setf unclassified-time (- total-time-in-files total-time-in-tags))))

    ;; Warning for unclassified time
    (when (and (invoicing--get-in-context :warn-on-unclassified-time)
               unclassified-time)
      (push (format "Found %d minutes of unclassified time." unclassified-time) warnings))

    ;; Warning for extra tags
    (let ((other-tags (cl-set-difference all-tags-in-files (mapcar (invoicing--agetter :tag) selected-tags) :test #'equalp)))
      (when (and (invoicing--get-in-context :warn-on-other-tags)
                 other-tags)
        (push (format "Found following tags that were not accounted for: %s." (s-join ", " other-tags)) warnings)))

    ;; Warning on multiple tags accounting for the same headline (TODO, also check if org-mode doesn't resolve this automagically)

    ;; Generate org-mode table
    ;; NOTE: ensure you include:
    ;; Constant items: # column, Tag, Line item name, hours, rate, net, [], total
    ;; Poland-relevant items: VAT %, VAT amount USD (if foreign), VAT amount PLN

    (let* ((table `(,headers
                    ;; TODO consider using ! and/or ^ here
                    hline
                    ;; List line items
                    ,@(mapcar (lambda (sum)
                                (mapcar (-compose (-rpartial #'funcall sum) #'invoicing-column-descriptor-value-generator)
                                        columns))
                              sums)
                    ;; List unallocated time
                    ,@(when (and unclassified-time (invoicing--get-in-context :include-unclassified-time))
                        (list (mapcar (-compose (-rpartial #'funcall `((:tag . "") (:name . "Other") (:time . ,unclassified-time))) #'invoicing-column-descriptor-value-generator)
                                      columns)))
                    hline
                    ;; TODO subtotals
                    hline
                    ,(list "#" "Totals")
                    ;; TODO currency conversion, if applicable; use $ if recalc row enabled
                    ;; Alternatively, #+CONSTANTS: might also work.
                    )))
      (cl-values table tblfms warnings))))

(defun invoicing--print-org-table-row (row)
  "Print `ROW' as a single line of org mode table."
  (if (eq row 'hline)
      (insert "|-\n")
    (insert "| "
            (s-join "|" (mapcar (lambda (el)
                                  (cond ((null el) "")
                                        ((stringp el) el)
                                        (t (format "%s" el))))
                                row))
            "|\n")))

(defun org-dblock-write:invoicing-items (params)
  "Generate an org mode table with invoice line items.
TODO document `PARAMS'.
This invokes the search through appropriate org mode agenda files
and collects sums of time spent, and other data necessary to generate
he table."
  ;; Ensure we have context ready.
  (invoicing--ensure-worksheet-context (plist-get params :seller) (plist-get params :customer))

  ;; Headline
  (when (plist-get params :name)
    (insert (format "#+NAME: %s\n" (plist-get params :name))))
  ;; TODO output something like #+CAPTION: Clock summary at [2019-02-05 Tue 17:48], for January 2019.

  ;; Render the main table and tblfms.
  (cl-multiple-value-bind (table tblfms warnings)
      ;; TODO pass through post-processing hooks.
      (invoicing--compute-line-items-table params)

    ;; Output the table
    ;; Doing it the hard way, because orgtbl-to-orgtbl strips recalc columns for some reason.
    (dolist (row table)
      (invoicing--print-org-table-row row))
    (org-table-align)

    (insert "#+TBLFM:"
            (s-join "::" tblfms))
    (insert "\n")
    ;; TODO force recalculate
    (dolist (warning warnings)
      (insert "# Warning: " warning "\n"))
    ;; TODO !!! ALSO OUTPUT TAGS NOT USED / WITH 0 TOTAL TIME, out of those explicitly requested.

    (values)))


;;; Other dblocks - TODO move elsewhere
(defun org-dblock-write:invoicing-headline-summary (params)
  "TODO"
  ;; TODO pawn the work off to clocktable, setting appropriate params
  (insert "TODO invoicing headline summary"))

(defun org-dblock-write:invoicing-data (params)
  "TODO"
  ;; TODO use context to compute keys & values for the final table, then render it
  (insert "TODO invoicing data"))

(defun org-dblock-write:invoicing-items-copy (params)
  "Generate a verbatim copy of a block named \"invoicing-items\".
`PARAMS' are ignored."
  (insert "**THIS TABLE WILL BE USED AS CANONICAL**\n") ;HACK to force indentation to work, and also a reminder
  (let ((point-before (point)))
    (insert (string-trim-left (save-excursion
                                (let* ((start (progn
                                                (goto-char (point-min))
                                                (re-search-forward (rx-to-string `(: "#+BEGIN: invoicing-items")) nil t)
                                                (forward-line)
                                                (point)))
                                       (end (progn
                                              (re-search-forward (rx-to-string `(: "#+END:")) nil t)
                                              (forward-line -1)
                                              (point))))
                                  (buffer-substring-no-properties start end)))))
    (org-indent-region point-before (point))))


;; template code -- TODO separate out to another file


;;; Entry point
(defun invoicing-create-invoice ()
  "Create invoice worksheet.
This is the entry point to invoicing process. The worksheet
created in temporary buffer allows user to inspect and adjust all
data that will go to the final invoice. Generating the final
invoice file is triggered from within the worksheet by (TODO
evaluating relevant code)."
  (interactive)
  ;; FIXME pull in block from somewhere too?
  ;;       Right now, changing block will require updates in _at least_ two places.
  (let ((buffer (generate-new-buffer "TODO-name-invoice.org")))
    (with-current-buffer buffer
      (org-mode)

      (invoicing--ensure-worksheet-context nil nil nil t t)

      (insert "#+TITLE: Invoice TODO number\n\n")

      ;; Insert all workbook data
      (insert "* Line items table\n"
              (if (invoicing--get-in-context :include-help-text)
                  "Here you can review and correct all line items as generated through scanning your Org Agenda.\n\n"
                "\n"))

      (insert "#+BEGIN: invoicing-items\n" ;TODO params
              "#+END:\n\n")

      (when (invoicing--get-in-context :include-copy-of-line-items)
        (insert (if (invoicing--get-in-context :include-help-text)
                    "A copy of the table above has been included for your convenience. *This* copy will be taken into account by final invoice generation process.\n\n"
                  "\n")
                "#+BEGIN: invoicing-items-copy\n"
                "#+END:\n\n"))

      (when (invoicing--get-in-context :include-summary-by-headline)
        (insert "* Summary of clocked tasks\n\n"
                "#+BEGIN: invoicing-headline-summary\n" ;TODO params
                "#+END:\n\n"))

      (insert "* Invoice data\n"
              (if (invoicing--get-in-context :include-help-text)
                  "Here are all the remaining values used to generate your invoice. You can review and modify them.\n\n"
                "\n")
              "#+BEGIN: invoicing-data\n"
              "#+END:")

      ;; Execute dynamic blocks to fetch all data
      (org-update-all-dblocks)

      ;; Reindent for nicer look
      (org-indent-region (point-min) (point-max)))

    (switch-to-buffer buffer)))


(provide 'invoicing)
;;; invoicing.el ends here
