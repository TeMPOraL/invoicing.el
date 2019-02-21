;;; invoicing.el --- Generate and manage invoices with Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jacek Złydach

;; Author: Jacek Złydach <temporal@temporal.pl>
;; Keywords: Org, agenda

;; TODO License

;;; Commentary:

;; TBD.


;;; Code:
;;;; Dependencies
(require 'cl-lib)
(require 'org)
(require 'org-table)
(require 'dash-functional)

;; TODO code in general


;; config definitions -- TODO separate out to another file
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
  :type 'directory)

;;; TODO could it be locally overridable per customer?
(defcustom invoicing-archive-directory nil
  "Path to a folder containing archived invoices.
This folder should store invoices that have already finished
their accounting life cycle. Contents of this folder are only
listed in order to determine the next applicable invoice number."
  :type 'directory)

;;; TODO could it be locally overridable per customer?
(defcustom invoicing-staging-directory nil
  "Path to a folder in which to store generated invoices.
Invoices in this folder are yet to be sent to the customer,
or yet to be sent to the accounting people."
  :type 'directory)


;; taxinfo code -- TODO separate out to another file


;; source-org code -- TODO separate out to another file


;; workbook generator code -- TODO separate out to another file

(defun invoicing--agetter (key)
  "Return an alist-getter for `KEY'.
An alist-getter is a function that accepts an alist,
and returns the value of `KEY'."
  (-compose #'cdr (-partial #'assoc key)))

(defvar invoicing--default-columns
  `(("" "" ,(-const "#") ,(-const nil))
    ("Tag" "Tag" ,#'car ,(-const nil))
    ("Line item name" "LineItem" ,(invoicing--agetter :name) ,(-const nil))
    ("Hours" "Hours" ,(invoicing--agetter :hours) ,(-const "TODO tblfm for total Hours"))
    ("Rate" "Rate" ,(invoicing--agetter :rate) ,(-const "TODO tblfm for total Rate"))
    ("Net" "Net" ,(-const nil) ,(-const `("TODO tblfm for row Net" "TODO tblfm for total Net")))
    ;; -- insertion point for custom formulas; TODO mark it maybe?
    ("Gross" "Gross" ,(-const nil) ,(-const `("TODO tblfm for row Gross" "TODO tblfm for total Gross")))))

(defun invoicing--compute-additional-line-item-table-columns (params)
  ;; TODO document
  ;; should return a list of columns, each being a list of:
  ;; - column name
  ;; - column variable name (for formulas)
  ;; - column getter - a function taking tag properties (with :hours being sum of hours in org mode), returning a value to put in the table
  ;; - column formula generator - a function taking the list of columns in order, returning a list of tblfms to add
  )

(defun org-dblock-write:invoice-items (params)
  "Generate an org mode table with invoice line items.
TODO document `PARAMS'."
  ;; Headline
  (when (plist-get params :name)
    (insert (format "#+NAME: %s\n" (plist-get params :name))))

  (let ((additional-columns (invoicing--compute-additional-line-item-table-columns params))))

  ;; Constant items: # column, Tag, Line item name, hours, rate, net, [], total
  ;; Poland-relevant items: VAT %, VAT amount USD (if foreign), VAT amount PLN

  ;; Contents
  ;; TODO 1. collect data
  ;; TODO 2. render via (orgtbl-to-orgtbl '(("foo" "bar" "baz") hline (1 2 3) (4 5 6)) nil)
  ;; TODO 2a. remember to insert # at the beginning!
  ;; TODO 2b. consider using ! and ^
  ;; TODO 2c. consider using $ for currency conversion rate

  ;; Generate formula row.
  ;; TODO output formula row
  )

;;orgtbl-to-orgtbl


;; template code -- TODO separate out to another file


(provide 'invoicing)
;;; invoicing.el ends here
