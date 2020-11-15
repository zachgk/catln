import React from 'react';

class Desugar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      error: null,
      isLoaded: false,
      objMap: [],
      classMap: [],
      notes: []
    };
  }

  componentDidMount() {
    fetch("/desugar")
      .then(res => res.json())
      .then(
        (result) => {
          if(result.length === 2) {
            this.setState({
              isLoaded: true,
              objMap: result[0][0],
              classMap: result[0][1],
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

  render() {
    const { error, isLoaded, objMap } = this.state;
    if (error) {
      return <div>Error: {error.message}</div>;
    } else if (!isLoaded) {
      return <div>Loading...</div>;
    } else {
      return (
          <div>
            <ObjMap objMap={objMap}/>
          </div>
      );
    }
  }
}

class ObjMap extends React.Component {
  render() {
    return (
      <ul>
        {this.props.objMap
         .sort((obj1, obj2) => obj1[0][2] < obj2[0][2])
         .map(obj =>
            <ObjArrows objas={obj} />
        )}
      </ul>
    );
  }
}

class ObjArrows extends React.Component {
  render() {
    const [obj, arrows] = this.props.objas;
    const [objM, objBasis, name, vars, args] = obj;

    let showVars;
    if(Object.keys(vars).length > 0) {
      showVars = (
          <span>
          &lt;
          {Object.keys(vars).map(v => v).join(', ')}
          &gt;
        </span>);
    }

    let showArgs;
    if(Object.keys(args).length > 0) {
      showArgs = (
        <span>
          (
          {Object.keys(args).map(arg => arg).join(', ')}
          )
        </span>);
    }

    let showArrows;
    if(Object.keys(arrows).length > 0) {
      showArrows = (
        <ul>
          {arrows.map(arrow => <Arrow arrow={arrow} />)}
        </ul>
      );
    }

    return (
        <li>
          <span>{name}</span>
          {showVars}
          {showArgs}
          {showArrows}
        </li>
    );
  }
}

class Arrow extends React.Component {
  render() {
    const [arrM, annots, guard, maybeExpr] = this.props.arrow;

    let showExpr;
    if(maybeExpr) {
      showExpr = ` = ${renderExpr(maybeExpr)}`;
    }

    return (
        <li>{renderGuard(guard)} -> {renderType(arrM)}{showExpr}</li>
    );
  }
}

function renderType(t) {
  switch(t.tag) {
  case "TopType":
    return "TopType";
  case "TypeVar":
    return t.contents.contents;
  case "SumType":
    let partials = t.contents.map(partialOptions => {
      let [partialName, options] = partialOptions;
      return options.map(partialData => {
        let [partialVars, partialProps, partialArgs] = partialData;

        let showVars = "";
        if(Object.keys(partialVars).length > 0) {
          showVars = (
              <span>
              &lt;
            {Object.keys(partialVars).map(v => v).join(', ')}
              &gt;
            </span>);
        }

        let showArgs = "";
        if(Object.keys(partialArgs).length > 0) {
          showArgs = (
              <span>
              (
                {Object.keys(partialArgs).map(arg => arg).join(', ')}
              )
            </span>);
        }

        return `${partialName.contents}${showVars}${showArgs}`;
      });
    }).flat();
    return partials.join(' | ');
  default:
    console.error("Unknown renderMeta");
    return "";
  }
}

function renderGuard(guard) {
  switch(guard.tag) {
  case "IfGuard":
    return `if (${renderExpr(guard.contents[0])})`;
  case "ElseGuard":
    return "else";
  case "NoGuard":
    return "";
  default:
    console.error("Unkwnon guard: ", guard);
    return "";
  }
}

function renderExpr(expr) {
  switch(expr.tag) {
  case "ICExpr":
    return "" + expr.contents[1].contents;
  case "IValue":
    return "" + expr.contents[1];
  case "IArg":
    return "" + expr.contents[1];
  case "ITupleApply":
    const [, [,base], arg, subExpr] = expr.contents;

    let showArg;
    if(arg) {
      showArg = `${arg} = `;
    }

    return `${renderExpr(base)}(${showArg}${renderExpr(subExpr)})`;
  default:
    console.error("Unknown renderExpr", expr);
  }
}

export default Desugar;
