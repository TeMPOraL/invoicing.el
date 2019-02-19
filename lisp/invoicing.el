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


;; template code -- TODO separate out to another file


(provide 'invoicing)
;;; invoicing.el ends here
