import React, {useState, useMemo, useContext, useRef} from 'react';

import { styled, alpha } from '@mui/material/styles';
import TextField from '@mui/material/TextField';
import Autocomplete from '@mui/material/Autocomplete';
import SearchIcon from '@mui/icons-material/Search';
import {useNavigate, useInRouterContext} from 'react-router-dom';

import {ConfigContext, callApi, useApi, useDebounced} from '../Common';

// The function the box calls, declared in stack/core/webdoc.ct. Named absolutely, so that which
// function is meant does not depend on what the page it is called against happens to name.
const SEARCH_FUN = '/Catln/Doc/searchWebdocs';

const SearchPill = styled('div')(({ theme }) => ({
  position: 'relative',
  borderRadius: theme.shape.borderRadius,
  backgroundColor: alpha(theme.palette.common.white, 0.15),
  '&:hover': {
    backgroundColor: alpha(theme.palette.common.white, 0.25),
  },
  marginRight: theme.spacing(2),
  marginLeft: 0,
  width: '100%',
  [theme.breakpoints.up('sm')]: {
    marginLeft: theme.spacing(3),
    width: 'auto',
  },
}));

const SearchIconWrapper = styled('div')(({ theme }) => ({
  padding: theme.spacing(0, 2),
  height: '100%',
  position: 'absolute',
  pointerEvents: 'none',
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
}));

const StyledTextField = styled(TextField)(({ theme }) => ({
  color: 'inherit',
  '& .MuiOutlinedInput-root': {
    padding: theme.spacing(1, 1, 1, 0),
    // vertical padding + font size from searchIcon
    paddingLeft: `calc(1em + ${theme.spacing(4)})`,
    transition: theme.transitions.create('width'),
    width: '100%',
    [theme.breakpoints.up('md')]: {
      width: '20ch',
    },
  },
}));

// How long a reader is left to go on typing before what they have typed is searched for. Long
// enough that typing a word is one search rather than one per letter, short enough not to be a wait
// of its own.
const SEARCH_DEBOUNCE_MS = 150;

// The pages a search covers, as a box that opens the one that is picked. This is the value of a
// #search annotation, which is the search for the empty query — every page there is — so the box it
// renders starts from that list and asks the compiler again as the reader types.
function Search(props) {
  // The editor renders a page's values into monaco view zones, which are React roots of their own
  // and so outside the router: useNavigate throws there, taking that root down with it. A box that
  // cannot open what it finds is not a search box, so it is left out rather than shown and broken.
  // Showing one there instead means giving the view zones the router, which is a change to how
  // every value is rendered rather than to this one.
  if (!useInRouterContext()) {
    return null;
  }
  return <SearchBox pages={props["/pages"]} />;
}

// What a value carrying a JSON list of pages holds, as that list. Absent for a box that was given
// no value to start from, which is the navbar's: it has nothing to show until the first search is
// answered.
function pagesOf(pages) {
  return pages ? JSON.parse(pages.contents) : null;
}

function SearchBox(props) {
  const {pages} = props;
  const config = useContext(ConfigContext);
  const navigate = useNavigate();
  const [navValue, setNavValue] = useState('');
  const query = useDebounced(navValue, SEARCH_DEBOUNCE_MS);

  // The search itself, done by the compiler over the pages it holds rather than here over a list of
  // them. The box sends what has been typed and is answered with what matches it, so what crosses is
  // the size of what is shown rather than the size of the doc server.
  //
  // Nothing is asked for until something has been typed, which is what that is worth: a box nobody
  // has typed into is every page there is, and sending that to draw a control nobody has used yet is
  // the doc server's whole list crossing on every page load — which is what searching here rather
  // than there is for.
  //
  // Named with the page the docs open at, the same way the sidebar is and for the same reason: what
  // it searches does not depend on the page it is written in, and naming one is what keeps each
  // search from compiling every page there is.
  const rootPage = config.data?.rootPage;
  const result = useApi(rootPage && query && callApi(SEARCH_FUN, {query}, rootPage));
  const answered = useMemo(() => pagesOf(result.data?.args?.["/pages"]), [result.data]);

  // What was last found stays on offer while the next search is being answered, so the list does not
  // empty and reopen under a reader between two letters.
  //
  // A box with nothing typed into it is back to what it started from rather than still showing what
  // was last searched for, since nothing is being searched for: that is whatever the value this was
  // rendered from carried, which for a #search is every page, and nothing at all for the navbar's.
  const initial = useMemo(() => pagesOf(pages), [pages]);
  const found = useRef(initial || []);
  if (answered) {
    found.current = answered;
  }
  const options = query ? found.current : (initial || []);

  const searchNavigate = (e, newValue) => {
    setNavValue('');
    if (newValue) {
      navigate(`/docs/${encodeURIComponent(newValue)}`);
      e.preventDefault();
    }
  };

  // No id: a page may hold a search of its own beside this one, and MUI builds the listbox id and
  // the aria references from it, so the two would collide. Left off, each box is given its own.
  //
  // Held at no selection: picking a page navigates to it rather than choosing it, so there is
  // nothing for the box to still be holding afterwards. Left to select, the pick would be written
  // back into the input and the box would sit there showing the page already being read.
  //
  // filterOptions passes the options through: they are already the answer to what was typed, and
  // MUI's own matching would search them a second time, by its rules rather than the compiler's,
  // and hide anything found by rules that are not its own.
  return (
    <SearchPill>
      <SearchIconWrapper>
        <SearchIcon />
      </SearchIconWrapper>
      <Autocomplete
        options={options}
        filterOptions={(x) => x}
        value={null}
        inputValue={navValue}
        onInputChange={(e,v) => setNavValue(v)}
        onChange={searchNavigate}
        renderInput={(params) => <StyledTextField {...params} />}
      />
    </SearchPill>
  );
}

// The navbar's box, which searches every page the doc server holds.
//
// It is the box alone rather than a #search annotation compiled to one: the box is not what a page
// says, and asking for it as an annotation meant compiling a program to be told a second time what
// the box already asks for itself.
function AppBarSearch() {
  return <SearchBox />;
}

export default Search;
export {AppBarSearch};
