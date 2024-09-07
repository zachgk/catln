import React, {useState, useContext} from 'react';

import { styled, alpha } from '@mui/material/styles';
import TextField from '@mui/material/TextField';
import Autocomplete from '@mui/material/Autocomplete';
import SearchIcon from '@mui/icons-material/Search';
import {useNavigate} from 'react-router-dom';

import {TocContext} from '../Common';

const Search = styled('div')(({ theme }) => ({
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

function AppBarSearch() {
  const tocResult = useContext(TocContext);
  const navigate = useNavigate();
  const [navValue, setNavValue] = useState('');

  let fileOptions = [];
  if (tocResult.isLoaded && tocResult.data) {
    fileOptions = tocResult.data.map(d => d[0]);
  }

  const searchNavigate = (e, newValue) => {
    setNavValue('');
    if (newValue) {
      navigate(`/docs/${encodeURIComponent(newValue)}`);
      e.preventDefault();
    }
  };

  return (
    <Search>
      <SearchIconWrapper>
        <SearchIcon />
      </SearchIconWrapper>
      <Autocomplete
        id="navigation-portal"
        options={fileOptions}
        inputValue={navValue}
        onInputChange={(e,v) => setNavValue(v)}
        onChange={searchNavigate}
        renderInput={(params) => <StyledTextField {...params} />}
      />
    </Search>
  );
}

export {AppBarSearch};
