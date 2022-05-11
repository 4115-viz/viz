import React, {useState, useRef} from 'react';
import './App.css';
import axios from 'axios';

const App = () => {
  
  const codeAreaRef = useRef();
  const [code, setCode] = useState('/*\nWelcome to VizOnline, the official online editor of the Viz programming language.\n*/\n\nfunc main(): none {\n    print("Welcome to the Viz programming language!");\n}');
  const [result, setResult] = useState("");
  const options = [{label: "Code Results", value: ""}, {label: "Build Process", value: "build"}, {label: "AST", value: "-a"}, {label: "SAST", value: "-s"}, {label: "LLVM IR", value: "-l"}, {label: "Tokens", value: "-ts"}];
  const [selectedOption, setSelectedOption] = useState(options[0].value);
  
  const run = async () => {
    if (code !== "") {
      const res = await axios.post("http://ec2-54-89-155-121.compute-1.amazonaws.com:5001/run", {content: code, language: "viz", args: selectedOption});
      // const res = await axios.post("http://localhost:5001/run", {content: code, language: "viz", args: selectedOption});
      let result = "";
      if (typeof res.data === 'object') {
        result = JSON.parse(JSON.stringify(res.data.error.stderr));
      } else {
        result = res.data;
      }
      setResult(result);
    }
  }

  return (
    <div className="page">
      <div style={{ display: 'flex', gap: '10px' }}>
        <a href="https://github.com/4115-viz/viz" target="_blank" rel="noopener noreferrer">Viz Compiler Source Code</a><br/><br/>
        <a href="https://github.com/4115-viz/viz/blob/master/writeups/LRM/Viz_LRM.pdf" target="_blank" rel="noopener noreferrer">Viz Language Reference Manual</a>
        <a href="https://github.com/4115-viz/viz/tree/master/test/programs" target="_blank" rel="noopener noreferrer">Sample Viz Programs</a>
      </div>
      <h3 className="title">Viz Online</h3>
      <div style={{ display: 'flex', height: '30px', justifyContent: 'center', alignItems: 'center', gap: '10px'}}>
        <h4>Result Format:</h4>
        <select
          value={selectedOption}
          onChange={e => setSelectedOption(e.target.value)}>
            {options.map(o => {
              return <option key={o.value} value={o.value}>{o.label}</option>
            })}
        </select>
        <button className="run-btn" onClick={run}>Run</button>
      </div>
        <div className="container">
          <textarea className="textArea"
            rows="20"
            cols="75"
            value={code}
            ref={codeAreaRef}
            onChange={(e) => {
              setCode(e.target.value);
            }}
            onKeyDown={(e) => {
              if (e.key === 'Tab') {
                e.preventDefault();
                const { selectionStart, selectionEnd } = e.target;
                const newText = code.substring(0, selectionStart) + '    ' + code.substring(selectionEnd, code.length);
                codeAreaRef.current.focus();
                codeAreaRef.current.value = newText;
                codeAreaRef.current.setSelectionRange(selectionStart + 4, selectionEnd + 4);
                setCode(newText);
              }
            }}
          />
          <textarea className="textArea"
            rows="20"
            cols="75"
            value={result}
            disabled={true}
          />
        </div>
    </div>
  );
}

export default App;
