;;; config/init-ai.el -*- lexical-binding: t; -*-
;; YOU DON'T NEED NONE OF THIS CODE FOR SIMPLE INSTALL
;; IT IS AN EXAMPLE OF CUSTOMIZATION.
(use-package! ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "German")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "llama3:8b-instruct-q8_0"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
	  '(("zephyr" . (make-llm-ollama
			 :chat-model "zephyr:7b-beta-q6_K"
			 :embedding-model "zephyr:7b-beta-q6_K"))
	    ("mistral" . (make-llm-ollama
			  :chat-model "mistral:7b-instruct-v0.2-q6_K"
			  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
	    ("mixtral" . (make-llm-ollama
			  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
			  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model "llama3:8b-instruct-q8_0"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
				       :chat-model "phi3:14b-medium-128k-instruct-q6_K"
				       :embedding-model "nomic-embed-text"))
  :config

  (defvar ellama-executive-summary-prompt-template "Text:\n%s\nYou are a CTO providing a technical assessment for a third party.  Provide a professional executive summary of the provided findings.  Highlight the issues, opportunities and recommendations use examples from the provided text where appropriate.")

  (defun ellama-executive-summary ()
    "Summarize selected region or current buffer."
    (interactive)
    (let ((text (if (region-active-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  (buffer-substring-no-properties (point-min) (point-max)))))
      (ellama-instant (format ellama-executive-summary-prompt-template text))))
  (defun ellama-prompt-and-execute (prompt)

    "Ask for a prompt before passing selected text too ollama"
    (interactive "M\\nAi prompt:")
    (let ((text (if (region-active-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  (buffer-substring-no-properties (point-min) (point-max)))))
      (ellama-instant (format "Text:\n%s\n%s" text prompt))))
  )
