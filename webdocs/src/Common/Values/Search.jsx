import React, {useState, useMemo} from 'react';

import { styled, alpha } from '@mui/material/styles';
import TextField from '@mui/material/TextField';
import Autocomplete from '@mui/material/Autocomplete';
import SearchIcon from '@mui/icons-material/Search';
import {useNavigate, useInRouterContext} from 'react-router-dom';

import {AnnotValue} from '../../DocsPage';

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

// The pages a search covers, as a box that opens the one that is picked. The list is the compiled
// value of a #search annotation, so nothing is fetched here. Which pages those are is the
// annotation's business: every page the doc server holds when it was evaluated unscoped, or the
// pages one page reaches when it was written in that page.
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

function SearchBox(props) {
  const {pages} = props;
  const options = useMemo(() => JSON.parse(pages.contents), [pages]);
  const navigate = useNavigate();
  const [navValue, setNavValue] = useState('');

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
  return (
    <SearchPill>
      <SearchIconWrapper>
        <SearchIcon />
      </SearchIconWrapper>
      <Autocomplete
        options={options}
        value={null}
        inputValue={navValue}
        onInputChange={(e,v) => setNavValue(v)}
        onChange={searchNavigate}
        renderInput={(params) => <StyledTextField {...params} />}
      />
    </SearchPill>
  );
}

// The navbar's box, which is a #search annotation evaluated over every loaded page. Named with no
// page on purpose, the same way the sidebar is: it searches every page the doc server holds, not
// the ones one page reaches, and the empty name also stops it from inheriting the page an /annot
// route is scoped to.
function AppBarSearch() {
  return <AnnotValue annot="#search" prgmName="" />;
}

export default Search;
export {AppBarSearch};
