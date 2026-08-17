import React, {useState, useMemo, useContext, useRef} from 'react';

import { styled, alpha } from '@mui/material/styles';
import TextField from '@mui/material/TextField';
import Autocomplete from '@mui/material/Autocomplete';
import Divider from '@mui/material/Divider';
import SearchIcon from '@mui/icons-material/Search';
import {useNavigate, useInRouterContext} from 'react-router-dom';

import {ConfigContext, annotLink, callApi, useApi, useDebounced} from '../Common';

// The functions the box calls, declared in stack/core/webdoc.ct. Named absolutely, so that which
// function is meant does not depend on what the page it is called against happens to name.
const SEARCH_FUN = '/Catln/Doc/searchWebdocs';
const DOC_PAGES_FUN = '/Catln/Doc/docPages';

// What a box is divided into. A section is a kind of thing there is to open rather than a place it
// was found: the pages that are annotations to run are asked for one way and the pages that are
// files another, and a reader picking one wants to know which they are picking.
//
// Ordered, because the options are handed over already grouped — MUI reads a group as ended when the
// next option belongs to another, so a section split across the list would be drawn twice.
const ANNOTATIONS = 'Annotations';
const PAGES = 'Pages';

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

// The name a section is drawn under. Sticky, so that a reader scrolling a long list of pages can
// still see which section they are scrolling.
const SectionHeader = styled('div')(({ theme }) => ({
  position: 'sticky',
  top: -8,
  zIndex: 1,
  padding: theme.spacing(0.75, 2),
  backgroundColor: theme.palette.background.paper,
  color: theme.palette.text.secondary,
  fontSize: theme.typography.pxToRem(11),
  fontWeight: theme.typography.fontWeightBold,
  letterSpacing: '0.08em',
  textTransform: 'uppercase',
}));

// An annotation is source a reader could have written themselves, so it is drawn as source. This is
// also what tells the two kinds apart at a glance where a query has matched both: a path and an
// annotation both begin with punctuation and are otherwise only told apart by reading them.
const AnnotOption = styled('span')({
  fontFamily: 'monospace',
});

// How wide the list under the box is at its narrowest. It grows to what is in it, so this is what a
// short answer is kept from shrinking to: a list narrower than the box it drops from reads as
// something other than that box's.
const LIST_MIN_WIDTH = 340;

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

// What was last answered, kept while the next answer is being waited for, so a section does not
// empty and refill under a reader between two letters. A ref rather than state because it is not
// what the box is showing but what it falls back on: writing it must not itself draw anything.
function useLastAnswered(answered, initial) {
  const held = useRef(initial);
  if (answered) {
    held.current = answered;
  }
  return held.current;
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
  // No pages are asked for until something has been typed, which is what that is worth: a box nobody
  // has typed into matches every page there is, and sending those to draw a control nobody has used
  // yet is the doc server's whole list crossing on every page load — which is what searching here
  // rather than there is for. The section below is asked for straight away instead, because what it
  // holds is a handful of entries rather than a listing that grows with the doc server.
  //
  // Named with the page the docs open at, the same way the sidebar is and for the same reason: what
  // it searches does not depend on the page it is written in, and naming one is what keeps each
  // search from compiling every page there is.
  const rootPage = config.data?.rootPage;
  const result = useApi(rootPage && query && callApi(SEARCH_FUN, {query}, rootPage));
  const answered = useMemo(() => pagesOf(result.data?.args?.["/pages"]), [result.data]);

  // The pages that are annotations rather than files. Asked for without waiting to be typed into,
  // which is the whole of what this section is for: a reader cannot search for a page they do not
  // know exists, and these are few and fixed where the pages below are the doc server's whole
  // listing. Narrowed by the same typing all the same, by the compiler, so both sections answer one
  // query by one rule.
  //
  // Scoped to the page the docs open at for the reason the search is: it names which program to
  // compile to answer, not which program the pages it names are about.
  const docPagesResult = useApi(rootPage && callApi(DOC_PAGES_FUN, {query}, rootPage));
  const docPagesAnswered = useMemo(() => pagesOf(docPagesResult.data?.args?.["/pages"]), [docPagesResult.data]);
  const docPages = useLastAnswered(docPagesAnswered, []);

  // A box with nothing typed into it is back to what it started from rather than still showing what
  // was last searched for, since nothing is being searched for: that is whatever the value this was
  // rendered from carried, which for a #search is every page, and nothing at all for the navbar's.
  const initial = useMemo(() => pagesOf(pages), [pages]);
  const found = useLastAnswered(answered, initial || []);
  const foundPages = query ? found : (initial || []);

  // Each option carries where it goes rather than what it is, so that which route a pick opens is
  // settled where the option is built and picking one is the same act whichever section it came
  // from. A section with nothing in it contributes nothing here, and so is not drawn at all.
  const options = useMemo(() => [
    ...docPages.map(p => ({section: ANNOTATIONS, label: p.dpLabel, to: annotLink(p.dpAnnot)})),
    ...foundPages.map(p => ({section: PAGES, label: p, to: `/docs/${encodeURIComponent(p)}`})),
  ], [docPages, foundPages]);

  const searchNavigate = (e, newValue) => {
    setNavValue('');
    if (newValue) {
      navigate(newValue.to);
      e.preventDefault();
    }
  };

  // The first section is drawn against the top of the list, so the rule is drawn above every section
  // after it rather than above each: a rule under nothing reads as the list having been cut off.
  const renderSection = (params) => (
    <li key={params.key}>
      {params.key !== 0 && <Divider component="div" />}
      <SectionHeader>{params.group}</SectionHeader>
      <ul style={{padding: 0}}>{params.children}</ul>
    </li>
  );

  const renderSectionOption = ({key, ...optionProps}, option) => (
    <li key={key} {...optionProps}>
      {option.section === ANNOTATIONS ? <AnnotOption>{option.label}</AnnotOption> : option.label}
    </li>
  );

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
  //
  // groupBy is what draws the sections, and draws only the ones there are: a group is made of the
  // options that fall in it, so a section nothing matched has no group and no heading rather than a
  // heading over nothing.
  //
  // The list is let out to the width of what is in it rather than held to the box's. A box is a box
  // wide enough to type a word into; what is offered under it is a path or an annotation written
  // out, which is longer than a word, and held to the box's width every one of them wrapped onto a
  // second line.
  return (
    <SearchPill>
      <SearchIconWrapper>
        <SearchIcon />
      </SearchIconWrapper>
      <Autocomplete
        options={options}
        filterOptions={(x) => x}
        getOptionLabel={(o) => o.label}
        groupBy={(o) => o.section}
        value={null}
        inputValue={navValue}
        onInputChange={(e,v) => setNavValue(v)}
        onChange={searchNavigate}
        renderGroup={renderSection}
        renderOption={renderSectionOption}
        renderInput={(params) => <StyledTextField {...params} />}
        slotProps={{
          // MUI sizes the popup to the box by writing the box's width onto it, so this is written
          // over that rather than beside it.
          popper: {style: {width: 'fit-content'}},
          paper: {sx: {minWidth: LIST_MIN_WIDTH, maxWidth: '90vw'}},
        }}
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
