require 'rake'

def remove_compiled_file(file)
  begin
    puts file + "c"
    FileUtils.rm(file + "c")
  rescue
  end
end

def execute(command)
  puts command
  system command
end

def compile_elisp(file, load_paths = [])
  execute "emacs -batch -Q -L . -L #{load_paths.join(" -L ")} -f batch-byte-compile #{file}"
end

task :compile do
  compile_elisp 'smart-newline.el'
end

task :test do
  test_files = Dir["test/*.el"].join(" -l ")
  execute "emacs -batch -Q -L . -L test -l vendor/cursor-test.el -l test/test-helper.el -l #{test_files} -f ert-run-tests-batch-and-exit"
end
