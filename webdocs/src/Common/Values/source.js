// Catln source for a value the compiler handed over, so that a value edited here can be written back
// as the declaration it was compiled from.
//
// A value is written out whole rather than rebuilt from the few fields an edit knows about: whatever
// it carries that the edit did not touch goes back as it came, and what the edit changed is the only
// difference. Which of its fields a value's line is written back from is the importer's to say, so
// nothing here knows what any field means.

// The source a value is written as. Names are written absolutely, as the source is parsed on its
// own rather than in the page it came from; an argument is written by the name it is declared with,
// without the slash the compiler keys it by.
export function valueSource(val) {
  switch (val.tag) {
  case 'StrVal':
    return JSON.stringify(val.contents);
  case 'IntVal':
  case 'FloatVal':
    return String(val.contents);
  case 'TupleVal': {
    const args = Object.entries(val.args || {});
    if (args.length === 0) {
      return val.name;
    }
    return `${val.name}(${args.map(([name, arg]) => `${name.replace(/^\//, '')}= ${valueSource(arg)}`).join(', ')})`;
  }
  default:
    throw new Error(`A ${val.tag} cannot be written back as source`);
  }
}

// A value with one field given another value, and the rest as they were.
export function withField(val, name, field) {
  return {...val, args: {...val.args, [name]: field}};
}

const integer = n => ({tag: 'IntVal', contents: n});

// A /Data/Maybe holding a value, or holding none.
export function some(value) {
  return {tag: 'TupleVal', name: '/Data/Maybe/Some', args: {'/value': value}};
}
export const nothing = {tag: 'TupleVal', name: '/Data/Maybe/Nothing', args: {}};

// A local Date as the /Data/Time/Date it is.
export function dateValue(date) {
  return {tag: 'TupleVal', name: '/Data/Time/Date', args: {
    '/year': integer(date.getFullYear()),
    '/month': integer(date.getMonth() + 1),
    '/day': integer(date.getDate())
  }};
}

// A span of a day, from one local Date to another, as the /Data/Time/TimeRange it is. Its end is the
// time of day the span ends at, which is before its start where it runs on past midnight.
export function timeRangeValue(start, end) {
  const timeOfDay = at => ({tag: 'TupleVal', name: '/Data/Time/TimeOfDay', args: {
    '/hour': integer(at.getHours()),
    '/minute': integer(at.getMinutes())
  }});
  return {tag: 'TupleVal', name: '/Data/Time/TimeRange', args: {'/start': timeOfDay(start), '/end': timeOfDay(end)}};
}
