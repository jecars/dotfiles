
;; -------------------- Debugging --------------------

;; python
(after! dap-mode
  (setq dap-python-debugger 'debugpy))


(setq realgud:pdb-command-name "python3 -m pdb"
      gud-pdb-command-name "python3 -m pdb")


;; rust
(after! dap-mode
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  ;; (require 'dap-lldb)
  (require 'dap-cpptools)
  ;; (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  ;; (dap-gdb-lldb-setup)
  (dap-cpptools-setup)
  (dap-register-debug-template "Rust::CppTools Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :environment []
                                     :program "${workspaceFolder}/target/debug/image2ascii"
                                     :cwd "${workspaceFolder}"
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}")))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))
