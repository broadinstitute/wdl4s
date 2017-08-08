cwlVersion: v1.0
class: Workflow
inputs:
- id: wf-pattern
  type: string
outputs:
- id: wf-cgrep-stdOut
  outputSource: '#cgrep/cgrep-stdOutf'
  type: File
- id: wf-wc-stdOut
  outputSource: '#wc/wc-stdOut'
  type: File
steps:
- id: ps
  in: []
  out:
  - ps-stdOut
  run:
    inputs: []
    outputs:
    - id: ps-stdOut
      outputBinding:
        glob: ps-stdOut.txt
      type: File
    class: CommandLineTool
    baseCommand: ps
    stdout: ps-stdOut.txt
- id: cgrep
  in:
  - id: pattern
    source: '#wf-pattern'
  - id: file
    source: ps/ps-stdOut
  out:
  - id: cgrep-stdOut
  run:
    inputs:
    - id: cg-pattern
      type: string
    - id: cg-file
      type: File
    outputs:
    - id: clt-cgrep-stdOut
      outputBinding:
        glob: cgrep-stdOut.txt
      type: File
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: grep
      shellQuote: false
    - valueFrom: $(inputs.pattern).
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: cgrep-stdOut.txt
- id: wc
  in:
  - id: file
    source: ps/ps-stdOut
  out:
  - id: wc-stdOut
  run:
    inputs:
    - id: wc-file
      type: File
    outputs:
    - id: clt-wc-stdOut
      outputBinding:
        glob: wc-stdOut.txt
      type: File
    class: CommandLineTool
    requirements:
    - class: ShellCommandRequirement
    - class: InlineJavascriptRequirement
    arguments:
    - valueFrom: cat
      shellQuote: false
    - valueFrom: $(inputs.file)
      shellQuote: false
    - valueFrom: '|'
      shellQuote: false
    - valueFrom: wc
      shellQuote: false
    - valueFrom: -l
      shellQuote: false
    stdout: wc-stdOut.txt
