import React from 'react';
import { Example, ExampleHeading } from './examples';

export const Sidebar: React.FC<{
  examples: Array<Example | ExampleHeading>;
  selected: Example;
  onSelect: (example: Example) => void;
}> = ({ examples, selected, onSelect }) => {
  return (
    <div style={{ fontFamily: 'sans-serif', fontSize: '16px' }}>
      {examples.map((example) => {
        if ('heading' in example) {
          return (
            <div
              key={example.heading}
              style={{
                padding: '.5rem',
                margin: '.5rem 0',
                fontSize: example.jumbo ? '24px' : undefined,
                fontWeight: 'bold',
              }}
            >
              {example.heading}
            </div>
          );
        } else {
          return (
            <div
              onClick={() => onSelect(example)}
              key={example.name}
              className="sidebar-item"
              style={{
                cursor: 'pointer',
                color: selected === example ? 'white' : '#aaa',
                backgroundColor: selected === example ? '#666' : '',
                padding: '.5em .5em .5em 1em',
                margin: '.5em 0',
              }}
            >
              {example.name}
            </div>
          );
        }
      })}
    </div>
  );
};
