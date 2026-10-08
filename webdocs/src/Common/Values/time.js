// Reading the values of /Data/Time (stack/core/time.ct) as they arrive from the compiler: tuples of
// integers, which are shown the way a note writes them.

function pad(n, width) {
  return String(n).padStart(width, '0');
}

function intArg(value, name) {
  return value.args[name].contents;
}

// A /Data/Maybe as the value it holds, or null where it holds none.
export function someValue(value) {
  return value && value.name === "/Data/Maybe/Some" ? value.args["/value"] : null;
}

// A /Data/Time/Date as YYYY-MM-DD, the way a note writes a day and the way a day travels in JSON.
export function dateText(value) {
  if (!value) return null;
  return `${pad(intArg(value, "/year"), 4)}-${pad(intArg(value, "/month"), 2)}-${pad(intArg(value, "/day"), 2)}`;
}

// A /Data/Time/TimeOfDay as HH:mm, on a 24-hour clock.
export function timeText(value) {
  if (!value) return null;
  return `${pad(intArg(value, "/hour"), 2)}:${pad(intArg(value, "/minute"), 2)}`;
}

// A /Data/Time/TimeRange as its two times with a dash between them.
export function rangeText(value) {
  if (!value) return null;
  return `${timeText(value.args["/start"])} - ${timeText(value.args["/end"])}`;
}
