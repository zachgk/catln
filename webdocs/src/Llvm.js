import React from 'react';

import SyntaxHighlighter from 'react-syntax-highlighter';

class Llvm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      error: null,
      isLoaded: false,
      llvm: "",
      notes: []
    };
  }

  runFetch() {
    fetch("/llvm")
      .then(res => res.json())
      .then(
        (result) => {
          if(result.length === 2) {
            this.setState({
              isLoaded: true,
              llvm: result[0],
              notes: result[1]
            });
          } else {
            this.setState({
              isLoaded: true,
              notes: result[0]
            });
          }
        },
        // Note: it's important to handle errors here
        // instead of a catch() block so that we don't swallow
        // exceptions from actual bugs in components.
        (error) => {
          this.setState({
            isLoaded: true,
            error
          });
        }
      );
  }

  componentDidMount() {
    this.runFetch();
  }

  render() {
    const { error, isLoaded, llvm } = this.state;
    if (error) {
      return <div>Error: {error.message}</div>;
    } else if (!isLoaded) {
      return <div>Loading...</div>;
    } else {
      return (
          <SyntaxHighlighter language="llvm">
            {llvm}
          </SyntaxHighlighter>
      );
    }
  }
}

export default Llvm;
