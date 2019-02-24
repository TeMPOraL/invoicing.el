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

(defcustom invoicing-add-recalc-column t
  "Include a column with recalculation marks in line items table?
Enabling this feature makes invoice line item table automatically
recalculate all table formulas when values are edited, without
needing to invoke recalculation manually."
  :group 'invoicing
  :type 'boolean)

;;(defcustom invoicing-warning-)


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
  :type '(repeat :tag nil (string :tag "Line"))) ;TODO Figure out a way to get rid of excess "Repeat:" in customize view.

(defcustom invoicing-broken-widget '()
  "Testing broken widget."
  :group 'invoicing
  :type 'invoicing-address)

(define-widget 'invoicing-account 'alist
  "TODO document"
  :type '(alist :key-type symbol)
  :tag "Bank account"
  :options '((:shortcode string)
             (:bank invoicing-address)
             (:number string)
             (:swift string)))

(define-widget 'invoicing-seller 'alist
  "Definition of a single invoicing identity.
Here you can specify the data that goes into \"seller\" field
on the invoice, as well as override some of the global invoicing
settings."
  ;; XXX need something to unbreak widget alignment in the UI!
  :tag "Seller"
  :type '(alist :key-type symbol)
  :options '((:shortcode string)
             (:full-name string)
             (:address invoicing-address)
             (:tax-id string)
             (:accounts (repeat invoicing-account))))

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


;; workbook generator code -- TODO separate out to another file

;;; Context variables.
(defvar invoicing-current-company nil
  "TODO")

(defvar invoicing-current-customer nil
  "TODO")

(defun invoicing--agetter (key)
  "Return an alist-getter for `KEY'.
An alist-getter is a function that accepts an alist,
and returns the value of `KEY'."
  (-partial #'alist-get key))

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

(defvar invoicing-additional-columns nil) ;TODO also probably should be defcustom

(defun invoicing--compute-line-item-table-columns (params) ;XXX maybe get rid of the params argument?
  "TODO document"
  `(,@(when invoicing-add-recalc-column
        (list (invoicing-make-column-descriptor " " " " (-const "#") (-const nil))))
    ;; TODO if possible, move that to defcustommable variable?
    ,@(mapcar (-partial #'apply #'invoicing-make-column-descriptor)
              `(("Tag" "Tag" ,(invoicing--agetter :tag) ,(-const nil))
                ("Line item name" "LineItem" ,(invoicing--agetter :name) ,(-const nil))
                ("Hours" "Hours" ,(-compose (-partial #'format "%12.6g") (invoicing--agetter :hours)) ,(-const `("TODO tblfm for total Hours")))
                ;; TODO format currency - potentially extensible!
                ("Rate" "Rate" ,(invoicing--agetter :rate) ,(-const `("TODO tblfm for total Rate")))
                ("Net" "Net" ,(-const nil) ,(-const `("TODO tblfm for row Net" "TODO tblfm for total Net")))))
    ,@invoicing-additional-columns
    ,(invoicing-make-column-descriptor "Gross" "Gross" (-const nil) (-const `("TODO tblfm for row Gross" "TODO tblfm for total Gross")))))

(defun invoicing--compute-line-items-table (params)
  "Actual computations for line item table.
TODO documentation
TODO document `PARAMS' or refer to dblock fun."
  (let* ((columns (invoicing--compute-line-item-table-columns params))
         (headers (mapcar #'invoicing-column-descriptor-name columns))
         (tblfms (-mapcat (-compose (-rpartial #'funcall columns) #'invoicing-column-descriptor-tblfm-generator) columns))
         ;; TODO 1. collect data
         ;; TODO run sums here
         ;; NOTE also collect warnings and "other tags"
         (sums '(((:tag . "tag1")
                  (:name . "Foo")
                  (:rate . "Rate1")
                  (:hours . "Hours1"))
                 ((:tag . "tag2")
                  (:name . "Bar")
                  (:rate . "Rate2")
                  (:hours . "Hours2"))))
         (warnings '("FooWarn" "BarWarn" "BazWarn"))

         ;; Assemble the table
         ;; TODO 2b. consider using ! and ^
         ;; TODO 2c. consider using $ for currency conversion rate
         ;; Constant items: # column, Tag, Line item name, hours, rate, net, [], total
         ;; Poland-relevant items: VAT %, VAT amount USD (if foreign), VAT amount PLN

         (table `(,headers
                  hline
                  ,@(mapcar (lambda (sum)
                              (mapcar (-compose (-rpartial #'funcall sum) #'invoicing-column-descriptor-value-generator)
                                      columns))
                            sums)
                  ;; TODO subtotals
                  hline
                  (,@(when invoicing-add-recalc-column
                       (list "#"))
                   "Totals"))))
    (cl-values table tblfms warnings)))

(defun invoicing--print-org-table-row (row)
  "Print `ROW' as a single line of org mode table."
  (if (eq row 'hline)
      (insert "|-\n")
    (insert "| "
            (s-join "|" row)
            "|\n")))

(defun org-dblock-write:invoice-items (params)
  "Generate an org mode table with invoice line items.
TODO document `PARAMS'.
This invokes the search through appropriate org mode agenda files
and collects sums of time spent, and other data necessary to generate
he table."
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

    (values)))


;; template code -- TODO separate out to another file


;;; Entry point
(defun invoicing-create-invoice ()
  "Create invoice worksheet.
This is the entry point to invoicing process. The worksheet
created in temporary buffer allows user to inspect and adjust all
data that will go to the final invoice. Generating the final
invoice file is triggered from within the workbook by (TODO
evaluating relevant code)."

  ;; TODO function params: customer, time block.
  ;; Customer needs to resolve to customer metadata, I think.

  ;; Will become buffer-local variables, I think.
  ;; OR MAYBE NOT, as it might interfere with agenda collection process!
  ;; Note we want to achieve two things here:
  ;; - have it set to correct values when interacting with Worksheet
  ;; - have it not hang around forever and possibly interfere with other Worksheets
  ;; Possible solution: have buffer-local variable and a normal variable that's automatically
  ;; set to the value of buffer-local variable whenever invoicing operations are happening.

  ;; TODO Create worksheet buffer.

  )


(provide 'invoicing)
;;; invoicing.el ends here
